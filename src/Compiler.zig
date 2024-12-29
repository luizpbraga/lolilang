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

pub fn leaveCurrentScope(c: *Compiler) !void {
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

    try c.currentInstruction().append(try std.mem.concat(c.allocator, u8, last_scope.instructions.items));
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
                // try c.enterScope();
                const len = block.statements.len;
                for (0.., block.statements) |i, _stmt| {
                    try c.compile(.{ .statement = _stmt });
                    if ((_stmt == .@"return" or _stmt == .@"break") and i < len - 1) {
                        std.debug.print("UnreachbleCode!\n\n", .{});
                        @panic("UnreachbleCode");
                    }
                }
                // const instruction = try c.leaveScope();
                // const pos = try c.addInstruction(instruction);
                // _ = pos;
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

                const rs = try c.symbols.?.define(func_stmt.name.value);

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

                {
                    try c.emit(.constant, &.{pos});
                    const symbol = try c.symbols.?.define(func_stmt.name.value);
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                }

                // TODO: think
                {
                    try c.emit(.constant, &.{pos});
                    const op: code.Opcode = if (rs.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{rs.index});
                }
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
                    const symbol = c.symbols.?.resolve(assignment.name.identifier.value) orelse {
                        return error.UndefinedVariable;
                    };

                    switch (assignment.operator) {
                        .@"=" => {
                            try c.compile(.{ .expression = assignment.value });
                        },

                        .@"+=" => {
                            try c.compile(.{ .expression = assignment.name });
                            try c.compile(.{ .expression = assignment.value });
                            try c.emit(.add, &.{});
                        },

                        .@"-=" => {
                            try c.compile(.{ .expression = assignment.name });
                            try c.compile(.{ .expression = assignment.value });
                            try c.emit(.sub, &.{});
                        },

                        .@"*=" => {
                            try c.compile(.{ .expression = assignment.name });
                            try c.compile(.{ .expression = assignment.value });
                            try c.emit(.mul, &.{});
                        },

                        else => return error.InvalidAssingmentOperation,
                    }

                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index}); // sapporra emite um pop
                    try c.emit(.null, &.{}); // will be pop, no integer overflow. why?
                    return;
                }

                if (assignment.name.* == .index) {
                    const left = assignment.name.index.left;
                    const index = assignment.name.index.index;
                    const value = assignment.value;

                    switch (assignment.operator) {
                        .@"=" => {
                            try c.compile(.{ .expression = left });
                            try c.compile(.{ .expression = index });
                            try c.compile(.{ .expression = value });
                        },

                        // .@"+=" => {
                        //     try c.compile(.{ .expression = left });
                        //     try c.compile(.{ .expression = index });
                        //     try c.compile(.{ .expression = value });
                        //     try c.emit(.add, &.{});
                        // },
                        //
                        // .@"-=" => {
                        //     try c.compile(.{ .expression = left });
                        //     try c.compile(.{ .expression = index });
                        //     try c.compile(.{ .expression = value });
                        //     try c.emit(.sub, &.{});
                        // },
                        //
                        // .@"*=" => {
                        //     try c.compile(.{ .expression = left });
                        //     try c.compile(.{ .expression = index });
                        //     try c.compile(.{ .expression = value });
                        //     try c.emit(.mul, &.{});
                        // },

                        else => return error.InvalidAssingmentOperation,
                    }

                    try c.emit(.index_set, &.{});
                    return;
                }

                if (assignment.name.* == .method) {
                    try c.compile(.{ .expression = assignment.name.method.caller });
                    const pos = try c.addConstants(.{ .tag = assignment.name.method.method.value });
                    try c.compile(.{ .expression = assignment.value });
                    try c.emit(.method_set, &.{pos});
                    return;
                }

                return error.InvalidAssingmentOperation;

                //
                //     const symbol = c.symbols.?.resolve(assignment.name.identifier.value) orelse {
                //         return error.UndefinedVariable;
                //     };
                //
            },

            .infix => |infix| {
                const operator = infix.operator;

                if (operator == .@"<") {
                    try c.compile(.{ .expression = infix.right });
                    try c.compile(.{ .expression = infix.left });
                    try c.emit(.gt, &.{});
                    return;
                }

                if (operator == .@"<=") {
                    try c.compile(.{ .expression = infix.right });
                    try c.compile(.{ .expression = infix.left });
                    try c.emit(.gte, &.{});
                    return;
                }

                try c.compile(.{ .expression = infix.left });
                try c.compile(.{ .expression = infix.right });
                const op: code.Opcode = switch (operator) {
                    .@"and" => .add,
                    .@"or" => .sub,
                    .@"+" => .add,
                    .@"-" => .sub,
                    .@"*" => .mul,
                    .@"/" => .div,
                    .@">" => .gt,
                    .@">=" => .gte,
                    .@"==" => .eq,
                    .@"!=" => .neq,
                    .@"%" => .mod,
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

            // TODO: rework
            .method => |method| {
                try c.compile(.{ .expression = method.caller });

                const symbol = c.symbols.?.resolve(method.method.value) orelse {
                    const pos = try c.addConstants(.{
                        .tag = method.method.value,
                    });
                    try c.emit(.constant, &.{pos});
                    try c.emit(.index_get, &.{});
                    return;
                };

                try c.emit(.method, &.{symbol.index});
            },

            .integer => |int| {
                const pos = try c.addConstants(.{
                    .integer = int.value,
                });
                try c.emit(.constant, &.{pos});
            },

            .enum_tag => |int| {
                const pos = try c.addConstants(.{
                    .tag = int.value,
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
                for (hash.pairs) |pair| {
                    const key, const val = pair;
                    try c.compile(.{ .expression = key });
                    // const val = pair[1];
                    try c.compile(.{ .expression = val });
                }

                try c.emit(.hash, &.{hash.pairs.len * 2});
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

            // TODO: think !!
            .range => |range| {
                try c.compile(.{ .expression = range.end });
                try c.compile(.{ .expression = range.start });
                try c.emit(.set_range, &.{});
            },

            .@"if" => |ifexp| {
                // if (ifexp.consequence.statements.len == 0) {
                //     try c.emit(.null, &.{});
                //     return;
                // }
                //
                try c.compile(.{ .expression = ifexp.condition });

                // since a new scope will be created, we need the counter
                const len = c.insLen();

                try c.enterScope();

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                if (ifexp.consequence.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    // compiling the consequence
                    try c.compile(.{ .statement = .{ .block = ifexp.consequence } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }

                // always jumb: null is returned
                const jum_pos = try c.emitPos(.jump, &.{9999});
                const after_consequence_position = c.insLen() + len;

                // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                if (ifexp.alternative) |alt| {
                    if (alt.statements.len == 0) {
                        try c.emit(.null, &.{});
                    } else {
                        try c.compile(.{ .statement = .{ .block = alt } });
                        // statements add a pop in the end, wee drop the last pop (if return)
                        if (c.lastInstructionIs(.pop)) c.removeLastPop();
                    }
                } else {
                    try c.emit(.null, &.{});
                }

                const after_alternative_pos = c.insLen() + len;
                // replases the 9999 (.jump) to the correct operand (after_alternative_pos);
                try c.changeOperand(jum_pos, after_alternative_pos);

                try c.leaveCurrentScope();
            },

            .match => |match| {
                var jumps_pos_list = std.ArrayList(usize).init(c.allocator);
                defer jumps_pos_list.deinit();

                for (match.arms) |arm| {
                    try c.compile(.{ .expression = match.value });
                    try c.compile(.{ .expression = arm.condition });
                    try c.emit(.eq, &.{});

                    const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                    if (arm.block.statements.len == 0) {
                        try c.emit(.null, &.{});
                    } else {
                        try c.compile(.{ .statement = .{ .block = arm.block } });
                        if (c.lastInstructionIs(.pop)) c.removeLastPop();
                    }

                    const jum_pos = try c.emitPos(.jump, &.{9999});
                    try jumps_pos_list.append(jum_pos);

                    const after_consequence_position = c.insLen();
                    try c.changeOperand(jump_if_not_true_pos, after_consequence_position);
                }

                if (match.else_block) |block| {
                    if (block.statements.len == 0) {
                        try c.emit(.null, &.{});
                    } else {
                        try c.compile(.{ .statement = .{ .block = block } });
                        if (c.lastInstructionIs(.pop)) c.removeLastPop();
                    }
                } else {
                    try c.emit(.null, &.{});
                }

                const after_alternative_pos = c.insLen();

                for (jumps_pos_list.items) |jum_pos| {
                    try c.changeOperand(jum_pos, after_alternative_pos);
                }
            },

            .@"for" => |forloop| {
                const jump_pos = c.insLen();

                try c.compile(.{ .expression = forloop.condition });

                const start_pos = c.insLen();

                try c.enterScope();
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

                try c.emit(.jump, &.{jump_pos});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                const after_consequence_position = c.insLen() + start_pos;
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                // // always jumb: null is returned
                try c.emit(.null, &.{});
                try c.leaveCurrentScope();
            },

            .for_range => |forloop| {
                const iterable = forloop.iterable;

                // load the range as a constant
                const pos = try c.addConstants(.null);
                try c.emit(.constant, &.{pos});
                try c.compile(.{ .expression = iterable });
                try c.emit(.to_range, &.{pos});

                const len = c.insLen();
                // in this block, the last instruction must be boolean
                {
                    try c.emit(.get_range, &.{pos});
                }

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.body.statements.len != 0) {
                    const symbol = try c.symbols.?.define(forloop.ident);
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
