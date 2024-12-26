const std = @import("std");
const ast = @import("ast.zig");
const code = @import("code.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const memory = @import("memory.zig");
const SymbolTable = @import("SymbolTable.zig");
const Scope = @import("Scope.zig");
const builtins = @import("builtins.zig");

/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();
allocator: std.mem.Allocator,

/// constants pool
constants: std.ArrayList(Value),
symbols: ?*SymbolTable,

scopes: std.ArrayList(Scope),
scope_index: usize,

pub fn init(alloc: std.mem.Allocator) !Compiler {
    const main_scope: Scope = .{
        .instructions = .init(alloc),
        .last_ins = undefined,
        .prev_ins = undefined,
    };

    var scopes: std.ArrayList(Scope) = .init(alloc);
    try scopes.append(main_scope);

    const s = try alloc.create(SymbolTable);
    errdefer alloc.destroy(s);
    s.* = .init(alloc);

    for (0.., builtins.list) |i, b| {
        _ = try s.defineBuiltin(i, b.name);
    }

    return .{
        .allocator = alloc,
        .constants = .init(alloc),
        .symbols = s,
        .scopes = scopes,
        .scope_index = 0,
    };
}

pub fn deinit(c: *Compiler) void {
    for (c.scopes.items) |scope| {
        for (scope.instructions.items) |ins| c.allocator.free(ins);
        scope.instructions.deinit();
    }

    c.scopes.deinit();
    c.constants.deinit();
    // GC
    if (c.symbols) |s| {
        s.deinit();
        c.allocator.destroy(s);
    }
}

fn loadSymbol(c: *Compiler, s: *SymbolTable.Symbol) !void {
    switch (s.scope) {
        .local => try c.emit(.getlv, &.{s.index}),
        .global => try c.emit(.getgv, &.{s.index}),
        .builtin => try c.emit(.getbf, &.{s.index}),
    }
}

pub fn enterScope(c: *Compiler) !void {
    try c.scopes.append(.{
        .prev_ins = undefined,
        .last_ins = undefined,
        .instructions = .init(c.allocator),
    });
    c.scope_index += 1;
    c.symbols = if (c.symbols) |s| try s.initEnclosed() else null;
}

pub fn leaveScope(c: *Compiler) !code.Instructions {
    c.scope_index -= 1;
    var last_scope = c.scopes.pop();
    defer {
        for (last_scope.instructions.items) |ins| c.allocator.free(ins);
        last_scope.instructions.deinit();
    }

    c.symbols = if (c.symbols) |s| b: {
        defer s.deinitEnclosed();
        break :b s.outer;
    } else null;

    return try std.mem.concat(c.allocator, u8, last_scope.instructions.items);
}

/// Walks the AST recursively and evaluate the node, and add it the the pool
pub fn compile(c: *Compiler, node: ast.Node) !void {
    switch (node) {
        .statement => |stmt| switch (stmt) {
            .program => |program| {
                for (program.statements.items) |s| {
                    try c.compile(.{ .statement = s });
                }
            },

            .exp_statement => |exp_stmt| {
                try c.compile(.{ .expression = exp_stmt.expression });
                try c.emit(.pop, &.{});
            },

            .block => |block| {
                const len = block.statements.len;
                for (0.., block.statements) |i, _stmt| {
                    try c.compile(.{ .statement = _stmt });
                    if ((_stmt == .@"return" or _stmt == .@"break") and i < len - 1) {
                        std.debug.print("UnreachbleCode!\n\n", .{});
                        @panic("UnreachbleCode");
                    }
                }
            },

            .@"var" => |var_stmt| {
                // var [var_stmt.name :: identifier] = [var_stmt.value :: *expression]
                // var_stmt.value is the right side expression
                try c.compile(.{ .expression = var_stmt.value });
                //const identifier = var_stmt.name;
                const symbol = try c.symbols.?.define(var_stmt.name.value);
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index});
            },

            .@"return" => |ret| {
                if (c.scope_index == 0) {
                    return error.InvalidReturnStatementOutsideAFunctionBody;
                }
                try c.compile(.{ .expression = ret.value });
                try c.emit(.retv, &.{});
            },

            // TODO: make it work in if/match/for
            .@"break" => |ret| {
                try c.compile(.{ .expression = ret.value });
                try c.emit(.brk, &.{});
            },

            .@"fn" => |func_stmt| {
                const func = func_stmt.func;
                try c.enterScope();
                errdefer if (c.symbols) |s| c.allocator.destroy(s);

                for (func.parameters) |parameter| {
                    _ = try c.symbols.?.define(parameter.value);
                }

                try c.compile(.{ .statement = .{ .block = func.body } });

                // return the last value if no return statement
                if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

                // return null
                if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

                const obj = try c.allocator.create(Object);
                errdefer c.allocator.destroy(obj);
                const num_locals = if (c.symbols) |s| s.def_number else 0;
                const instructions = try c.leaveScope();
                obj.type = .{
                    .function = .{
                        .instructions = instructions,
                        .num_locals = num_locals,
                        .num_parameters = func.parameters.len,
                    },
                };

                const pos = try c.addConstants(.{ .obj = obj });
                try c.emit(.constant, &.{pos});
                const symbol = try c.symbols.?.define(func_stmt.name.value);
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index});
                // try c.emit(.null, &.{});
            },

            else => return error.InvalidStatemend,
        },

        .expression => |exp| switch (exp.*) {
            .identifier => |ident| {
                const symbol = c.symbols.?.resolve(ident.value) orelse {
                    return error.UndefinedVariable;
                };
                try c.loadSymbol(symbol);
            },

            // TODO: not fully implemented
            .assignment => |assignment| {
                if (assignment.operator == .@":=") {
                    try c.compile(.{ .expression = assignment.value });
                    //const identifier = var_stmt.name;
                    const symbol = try c.symbols.?.define(assignment.name.identifier.value);
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                    try c.emit(.null, &.{});
                    return;
                }

                if (assignment.name.* == .identifier) {
                    try c.compile(.{ .expression = assignment.name });
                    try c.compile(.{ .expression = assignment.value });
                    const symbol = c.symbols.?.resolve(assignment.name.identifier.value) orelse {
                        return error.UndefinedVariable;
                    };
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                    // try c.emit(.null, &.{});
                }

                if (assignment.name.* == .index) {
                    try c.compile(.{ .expression = assignment.name.index.left });
                    try c.compile(.{ .expression = assignment.name.index.index });
                    try c.compile(.{ .expression = assignment.value });
                    try c.emit(.index_set, &.{});
                }

                //
                //     const symbol = c.symbols.?.resolve(assignment.name.identifier.value) orelse {
                //         return error.UndefinedVariable;
                //     };
                //
                //     switch (assignment.operator) {
                //         .@"+=" => {
                //             try c.compile(.{ .expression = assignment.name });
                //             try c.compile(.{ .expression = assignment.value });
                //             try c.emit(.add, &.{});
                //         },
                //
                //         .@"-=" => {
                //             try c.compile(.{ .expression = assignment.name });
                //             try c.compile(.{ .expression = assignment.value });
                //             try c.emit(.sub, &.{});
                //         },
                //
                //         .@"*=" => {
                //             try c.compile(.{ .expression = assignment.name });
                //             try c.compile(.{ .expression = assignment.value });
                //             try c.emit(.mul, &.{});
                //         },
                //
                //         .@"=" => {
                //             try c.compile(.{ .expression = assignment.value });
                //         },
                //
                //         else => return error.InvalidAssingmentOperation,
                //     }
                //
                //     const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                //     try c.emit(op, &.{symbol.index}); // sapporra emite um pop
                //     try c.emit(.null, &.{}); // will be pop, no integer overflow. why?
            },

            // rework
            .method => |method| {
                try c.compile(.{ .expression = method.caller });
                const symbol = c.symbols.?.resolve(method.method.value) orelse {
                    return error.undefinedSymbol;
                };
                try c.emit(.method, &.{symbol.index});
            },

            .infix => |infix| {
                const operator = infix.operator;

                if (operator == .@"<") {
                    try c.compile(.{ .expression = infix.right });
                    try c.compile(.{ .expression = infix.left });
                    try c.emit(.gt, &.{});
                    return;
                }

                try c.compile(.{ .expression = infix.left });
                try c.compile(.{ .expression = infix.right });
                const op: code.Opcode = switch (operator) {
                    .@"+" => .add,
                    .@"-" => .sub,
                    .@"*" => .mul,
                    .@"/" => .div,
                    .@">" => .gt,
                    .@"==" => .eq,
                    .@"!=" => .neq,
                    else => return error.UnknowOperator,
                };
                try c.emit(op, &.{});
            },

            .prefix => |prefix| {
                const operator = prefix.operator;

                try c.compile(.{ .expression = prefix.right });

                switch (operator) {
                    .@"!" => try c.emit(.not, &.{}),
                    .@"-" => try c.emit(.min, &.{}),
                    else => unreachable,
                }
            },

            .index => |index| {
                try c.compile(.{ .expression = index.left });
                try c.compile(.{ .expression = index.index });
                try c.emit(.index_get, &.{});
            },

            .integer => |int| {
                const pos = try c.addConstants(.{
                    .integer = int.value,
                });
                try c.emit(.constant, &.{pos});
            },

            .char => |char| {
                const pos = try c.addConstants(.{
                    .char = char.value,
                });
                try c.emit(.constant, &.{pos});
            },

            .float => |float| {
                const pos = try c.addConstants(.{
                    .float = float.value,
                });
                try c.emit(.constant, &.{pos});
            },

            .string => |str| {
                const cvalue = try c.allocator.dupe(u8, str.value);
                errdefer c.allocator.free(cvalue);
                const obj = try c.allocator.create(Object);
                errdefer c.allocator.destroy(obj);
                obj.type = .{ .string = cvalue };
                const pos = try c.addConstants(.{ .obj = obj });
                try c.emit(.constant, &.{pos});
            },

            .array => |array| {
                for (array.elements) |element| {
                    try c.compile(.{ .expression = element });
                }
                try c.emit(.array, &.{array.elements.len});
            },

            .hash => |hash| {
                var iterator = hash.pairs.iterator();

                while (iterator.next()) |entry| {
                    const key = entry.key_ptr.*;
                    try c.compile(.{ .expression = key });
                    const val = entry.value_ptr.*;
                    try c.compile(.{ .expression = val });
                }

                try c.emit(.hash, &.{hash.pairs.count() * 2});
            },

            .call => |call| {
                try c.compile(.{ .expression = call.function });

                for (call.arguments) |arg| {
                    try c.compile(.{ .expression = arg });
                }

                try c.emit(.call, &.{call.arguments.len});
            },

            .function => |func| {
                try c.enterScope();
                errdefer if (c.symbols) |s| c.allocator.destroy(s);

                for (func.parameters) |parameter| {
                    _ = try c.symbols.?.define(parameter.value);
                }

                try c.compile(.{ .statement = .{ .block = func.body } });

                // return the last value if no return statement
                if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

                // return null
                if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

                const obj = try c.allocator.create(Object);
                errdefer c.allocator.destroy(obj);
                const num_locals = if (c.symbols) |s| s.def_number else 0;
                const instructions = try c.leaveScope();
                obj.type = .{
                    .function = .{
                        .instructions = instructions,
                        .num_locals = num_locals,
                        .num_parameters = func.parameters.len,
                    },
                };

                const pos = try c.addConstants(.{ .obj = obj });
                try c.emit(.constant, &.{pos});
            },

            .boolean => |boolean| {
                const op: code.Opcode = if (boolean.value) .true else .false;
                try c.emit(op, &.{});
            },

            .null => {
                try c.emit(.null, &.{});
            },

            // todo: think !!
            .range => |range| {
                if (range.start.* != .integer) return error.InvalidRangeIndex;
                if (range.end.* != .integer) return error.InvalidRangeIndex;
                try c.compile(.{ .expression = range.start });
                try c.compile(.{ .expression = range.end });
                try c.emit(.range, &.{@intCast(range.end.integer.value - range.start.integer.value)});
            },

            // BUG: empty blocks (non expressions) overflows
            // PROPOSE: every block must return null, unless a break statement is found
            .@"if" => |ifexp| {
                // AST if (condition) { consequence } else { alternative }
                //
                // compiling the condition
                try c.compile(.{ .expression = ifexp.condition });

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence

                if (ifexp.consequence.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compile(.{ .statement = .{ .block = ifexp.consequence } });
                }

                // statements add a pop in the end, wee drop the last pop (if return)
                if (c.lastInstructionIs(.pop)) {
                    c.removeLastPop();
                }

                // always jumb: null is returned
                const jum_pos = try c.emitPos(.jump, &.{9999});

                const after_consequence_position = c.insLen();
                // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                if (ifexp.alternative) |alt| {
                    try c.compile(.{ .statement = .{ .block = alt } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                } else {
                    try c.emit(.null, &.{});
                }

                const after_alternative_pos = c.insLen();
                // replases the 9999 (.jump) to the correct operand (after_alternative_pos);
                try c.changeOperand(jum_pos, after_alternative_pos);
            },

            else => |e| {
                std.log.err("find an invalid/not handled Expression: {}\n", .{e});
                return error.InvalidExpression;
            },

            .@"for" => |forloop| {
                const len = c.insLen();
                try c.compile(.{ .expression = forloop.condition });

                // fake insrtuction position
                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.consequence.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compile(.{ .statement = .{ .block = forloop.consequence } });
                }

                // statements add a pop in the end, wee drop the last pop (if return)
                if (c.lastInstructionIs(.pop)) c.removeLastPop();

                try c.emit(.jump, &.{len});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                const after_consequence_position = c.insLen();
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                // // always jumb: null is returned
                try c.emit(.null, &.{});
            },

            .for_range => |forloop| {
                const iterable = forloop.iterable;

                const pos = try c.addConstants(.null);
                try c.emit(.constant, &.{pos});

                try c.compile(.{ .expression = iterable });
                try c.emit(.range, &.{pos});

                const symbol = try c.symbols.?.define(forloop.ident);
                //std.debug.print("name:{s} {?}\n", .{ forloop.ident, c.symbols.?.store.get(forloop.ident) });

                const len = c.insLen();
                // in this block, the last instruction must be boolean
                {
                    try c.emit(.get_range, &.{pos});
                }

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.body.statements.len != 0) {
                    try c.emit(.setgv, &.{symbol.index});
                    try c.compile(.{ .statement = .{ .block = forloop.body } });
                }

                // statements add a pop in the end, wee drop the last pop (if return)
                if (c.lastInstructionIs(.pop)) c.removeLastPop();

                try c.emit(.jump, &.{len});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                const after_consequence_position = c.insLen();

                // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                // // always jumb: null is returned
                try c.emit(.null, &.{});

                _ = c.symbols.?.store.swapRemove(forloop.ident);
            },
        },
    }
}

fn insLen(c: *Compiler) usize {
    var len: usize = 0;
    for (c.currentInstruction().items) |ins| len += ins.len;
    return len;
}

fn currentInstruction(c: *Compiler) *std.ArrayList(code.Instructions) {
    return &c.scopes.items[c.scope_index].instructions;
}

/// FIX
fn removeLastPop(c: *Compiler) void {
    c.allocator.free(c.currentInstruction().pop());
    c.scopes.items[c.scope_index].last_ins = c.scopes.items[c.scope_index].prev_ins;
}

fn addConstants(c: *Compiler, val: Value) !usize {
    try c.constants.append(val);
    return c.constants.items.len - 1;
}

fn addInstruction(c: *Compiler, ins: code.Instructions) !usize {
    const pos_new_ins = c.currentInstruction().items.len;
    try c.currentInstruction().append(ins);
    return pos_new_ins;
}

fn replaceInstruction(c: *Compiler, pos: usize, new_ins: []u8) !void {
    c.allocator.free(c.currentInstruction().orderedRemove(pos));
    try c.currentInstruction().insert(pos, new_ins);
}

fn replaceLastPopWithReturn(c: *Compiler) !void {
    const last_pos = c.scopes.items[c.scope_index].last_ins.pos;
    try c.replaceInstruction(last_pos, try code.makeBytecode(c.allocator, .retv, &.{}));
    c.scopes.items[c.scope_index].last_ins.opcode = .retv;
}

/// replaces the instruction
fn changeOperand(c: *Compiler, op_pos: usize, operand: usize) !void {
    const op: code.Opcode = @enumFromInt(c.currentInstruction().items[op_pos][0]);
    const new_ins = try code.makeBytecode(c.allocator, op, &.{operand});
    try c.replaceInstruction(op_pos, new_ins);
}

/// generate a instruction and add it to a pool
pub fn emit(c: *Compiler, op: code.Opcode, operants: []const usize) !void {
    const ins = try code.makeBytecode(c.allocator, op, operants);
    const pos = try c.addInstruction(ins);
    c.setLastInstruction(op, pos);
}

pub fn emitPos(c: *Compiler, op: code.Opcode, operants: []const usize) !usize {
    const ins = try code.makeBytecode(c.allocator, op, operants);
    const pos = try c.addInstruction(ins);
    c.setLastInstruction(op, pos);
    return pos;
}

fn setLastInstruction(c: *Compiler, op: code.Opcode, pos: usize) void {
    c.scopes.items[c.scope_index].prev_ins = c.scopes.items[c.scope_index].last_ins;
    c.scopes.items[c.scope_index].last_ins = .{ .opcode = op, .pos = pos };
}

fn lastInstructionIs(c: *Compiler, op: code.Opcode) bool {
    if (c.currentInstruction().items.len == 0) return false;
    return c.scopes.items[c.scope_index].last_ins.opcode == op;
}

pub fn bytecode(c: *Compiler) !Bytecode {
    const ins = try std.mem.concat(c.allocator, u8, c.currentInstruction().items);
    errdefer c.allocator.free(ins);
    return .{ .constants = c.constants.items, .instructions = ins };
}

/// compiler generated bytecode
pub const Bytecode = struct {
    constants: []Value,
    instructions: code.Instructions,

    pub fn deinit(b: *Bytecode, c: *const Compiler) void {
        c.allocator.free(b.instructions);
    }
};

test {
    // _ = @import("compiler_test.zig");
    // _ = SymbolTable;
    // _ = Scope;
}
