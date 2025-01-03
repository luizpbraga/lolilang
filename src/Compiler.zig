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
/// testing the logic
loop: struct { top: usize = 0, start: usize = 0 } = .{},

struct_fields: std.ArrayList(struct { index: usize, fields: [][]const u8 = &.{} }),
struct_index: usize = 0,

const Loop = struct {
    loops: [10]struct { start: usize, end: usize },
    idx: usize = 0,
};

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
        .struct_fields = .init(alloc),
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
        .free => try c.emit(.getfree, &.{s.index}),
        .function => try c.emit(.current_closure, &.{}),
    }
}

/// TODO: make instructions a pointer
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
                // try c.enterScope();
                // const len = block.statements.len;
                for (block.statements) |_stmt| {
                    try c.compile(.{ .statement = _stmt });
                    // if ((_stmt == .@"return" or _stmt == .@"break") and i < len - 1) {
                    //     std.debug.print("UnreachbleCode!\n\n", .{});
                    //     @panic("UnreachbleCode");
                    // }
                }
                // try c.leaveScope();
            },

            .@"var" => |var_stmt| {
                const symbol = try c.symbols.?.define(var_stmt.name.value);
                try c.compile(.{ .expression = var_stmt.value });
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
                try c.emit(.jump, &.{c.loop.top});
            },

            .@"continue" => |ret| {
                try c.compile(.{ .expression = ret.value });
                try c.emit(.jump, &.{c.loop.start});
            },

            .@"fn" => |func_stmt| {
                const symbol = try c.symbols.?.define(func_stmt.name.value);
                const func = func_stmt.func;

                try c.enterScope();

                // integer overflow! With the fallowing line commented,
                // the internal function error like UndefinedVariable is returned
                // errdefer if (c.symbols) |s| c.allocator.destroy(s);
                // but leaks memory

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
                errdefer c.allocator.free(instructions);

                obj.type = .{
                    .function = .{
                        .instructions = instructions,
                        .num_locals = num_locals,
                        .num_parameters = func.parameters.len,
                    },
                };

                const pos = try c.addConstants(.{ .obj = obj });

                try c.emit(.constant, &.{pos});
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index});
            },

            else => return error.InvalidStatemend,
        },

        .expression => |exp| switch (exp.*) {
            .identifier => |ident| {
                const symbol = try c.symbols.?.resolve(ident.value) orelse {
                    return error.UndefinedVariable;
                };
                try c.loadSymbol(symbol);
            },

            // TODO: not fully implemented
            .assignment => |assignment| {
                if (assignment.operator == .@":=") {
                    const symbol = try c.symbols.?.define(assignment.name.identifier.value);
                    try c.compile(.{ .expression = assignment.value });
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                    try c.emit(.null, &.{});
                    return;
                }

                if (assignment.name.* == .identifier) {
                    const symbol = try c.symbols.?.resolve(assignment.name.identifier.value) orelse {
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
                const symbol = try c.symbols.?.resolve(method.method.value) orelse {
                    const pos = try c.addConstants(.{
                        .tag = method.method.value,
                    });
                    try c.emit(.constant, &.{pos});
                    try c.emit(.index_get, &.{});
                    return;
                };
                try c.emit(.method_get, &.{symbol.index});
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

            .integer => |int| {
                const pos = try c.addConstants(.{
                    .integer = int.value,
                });
                try c.emit(.constant, &.{pos});
            },

            .tag => |tag| {
                const pos = try c.addConstants(.{
                    .tag = tag.value,
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

            .instance => |instance| {
                try c.compile(.{ .expression = instance.@"struct" });
                for (instance.fields) |field| {
                    const value = field.value;
                    try c.compile(.{ .expression = value });
                }
                try c.emit(.instance, &.{instance.fields.len});
            },

            .function => |func| {
                try c.enterScope();
                errdefer if (c.symbols) |s| c.allocator.destroy(s);

                // TODO: ast
                if (func.name) |name| {
                    _ = try c.symbols.?.defineFunctionName(name.value);
                }

                for (func.parameters) |parameter| {
                    _ = try c.symbols.?.define(parameter.value);
                }

                try c.compile(.{ .statement = .{ .block = func.body } });

                // return the last value if no return statement
                if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

                // return null
                if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

                const free_symbols = c.symbols.?.frees;
                const obj = try c.allocator.create(Object);
                errdefer c.allocator.destroy(obj);
                const num_locals = if (c.symbols) |s| s.def_number else 0;

                for (free_symbols.items) |symb| {
                    try c.loadSymbol(symb);
                }
                const num_free_symbols = free_symbols.items.len;

                const instructions = try c.leaveScope();
                errdefer c.allocator.free(instructions);

                obj.type = .{ .closure = .{
                    .func = .{
                        .instructions = instructions,
                        .num_locals = num_locals,
                        .num_parameters = func.parameters.len,
                    },
                } };

                const pos = try c.addConstants(.{ .obj = obj });
                try c.emit(.closure, &.{ pos, num_free_symbols });
            },

            .@"if" => |ifexp| {
                try c.compile(.{ .expression = ifexp.condition });

                // since a new scope will be created, we need the counter
                // const len = c.insLen();
                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                if (ifexp.consequence.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    // compiling the consequence
                    try c.compile(.{ .statement = .{ .block = ifexp.consequence } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }

                // always jump: null is returned
                const jump_pos = try c.emitPos(.jump, &.{9999});
                const after_consequence_position = c.insLen();

                // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
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

                const after_alternative_pos = c.insLen();
                // Replaces the 9999 (.jump) to the correct operand (after_alternative_pos);
                try c.changeOperand(jump_pos, after_alternative_pos);
            },

            .match => |match| {
                var jumps_pos_list = std.ArrayList(usize).init(c.allocator);
                defer jumps_pos_list.deinit();

                // try c.enterScope();

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
                c.loop.start = jump_pos;

                try c.compile(.{ .expression = forloop.condition });

                // fake instruction position
                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.consequence.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compile(.{ .statement = .{ .block = forloop.consequence } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }

                try c.emit(.jump, &.{jump_pos});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                // const after_consequence_position = c.insLen() + start_pos;
                const after_consequence_position = c.insLen();
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                c.loop.top = after_consequence_position;
                try c.emit(.null, &.{});
            },

            .for_range => |forloop| {
                // cast the expression as a range and load it as a constant
                const pos = try c.addConstants(.null);
                try c.emit(.constant, &.{pos});
                try c.compile(.{ .expression = forloop.iterable });
                try c.emit(.to_range, &.{pos});

                const jump_pos = c.insLen();
                c.loop.start = jump_pos;
                // in this block, the last instruction must be boolean
                {
                    try c.emit(.get_range, &.{pos});
                }

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.body.statements.len != 0) {
                    const symbol = try c.symbols.?.define(forloop.ident);
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                    try c.compile(.{ .statement = .{ .block = forloop.body } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }

                try c.emit(.jump, &.{jump_pos});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                const after_consequence_position = c.insLen();
                c.loop.top = after_consequence_position;

                // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                // // always jump: null is returned
                try c.emit(.null, &.{});
            },

            .@"struct" => |st| {
                const fields = st.fields;
                const descs = st.desc;

                for (fields) |field| {
                    const ident = field.name;
                    const value = field.value;

                    const pos = try c.addConstants(.{ .tag = ident.value });
                    try c.emit(.constant, &.{pos});
                    try c.compile(.{ .expression = value });
                }

                for (descs) |desc| {
                    _ = desc;
                }

                try c.emit(.@"struct", &.{ c.struct_index, fields.len });
                c.struct_index += 1;
            },

            // else => |exx| {
            //     std.debug.print("{}\n", .{exx});
            //     @panic("Invalid Expression");
            // },
        },
    }
}

fn insLen(c: *Compiler) usize {
    var len: usize = 0;
    for (c.currentInstruction().items) |ins| len += ins.len;
    return len;
}

fn insLenRec(c: *Compiler) usize {
    var len: usize = 0;
    var si = c.scope_index;
    while (true) {
        for (c.scopes.items[si].instructions.items) |ins| len += ins.len;
        if (si == 0) break;
        si -= 1;
    }
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

// pub fn enterScope(c: *Compiler) !void {
//     // const idx = c.currentInstruction().items.len;
//     try c.scopes.append(.{
//         .prev_ins = undefined,
//         .last_ins = undefined,
//         .instructions = .init(c.allocator),
//         // .instructions = c.currentInstruction(),
//         // .idx = idx,
//     });
//     c.scope_index += 1;
//     c.symbols = if (c.symbols) |s| try s.initEnclosed() else null;
// }

// pub fn leaveScope(c: *Compiler) !void {
//     c.scope_index -= 1;
//     _ = c.scopes.pop();
//     c.symbols = if (c.symbols) |s| b: {
//         defer s.deinitEnclosed();
//         break :b s.outer;
//     } else null;
// }
//
// pub fn leaveScopeFn(c: *Compiler) !code.Instructions {
//     c.scope_index -= 1;
//     const last_scope = c.scopes.pop();
//
//     defer {
//
//     }
//
//     c.symbols = if (c.symbols) |s| b: {
//         defer s.deinitEnclosed();
//         break :b s.outer;
//     } else null;
//
//     const ins = try std.mem.concat(c.allocator, u8, last_scope.instructions.items[last_scope.idx..]);

// var len = last_scope.instructions.items.len - 1;
// while (last_scope.idx < len) {
//     const el = last_scope.instructions.orderedRemove(len);
//     c.allocator.free(el);
//     len -= 1;
// }
//     return ins;
// }
