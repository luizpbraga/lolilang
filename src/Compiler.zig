/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();
/// constants pool
allocator: std.mem.Allocator,
constants: std.ArrayList(object.Object),
instructions: std.ArrayList(code.Instructions),
last_ins: EmittedInstruction,
prev_ins: EmittedInstruction,
symbols: SymbolTable,

pub fn init(alloc: anytype) Compiler {
    return .{
        .allocator = alloc,
        .constants = .init(alloc),
        .instructions = .init(alloc),
        .symbols = .init(alloc),
        .last_ins = undefined,
        .prev_ins = undefined,
    };
}

pub fn deinit(c: *Compiler) void {
    // std.debug.print("{x}\n", .{c.instructions.items});
    for (c.instructions.items) |ins| c.allocator.free(ins);
    c.constants.deinit();
    c.instructions.deinit();
    c.symbols.deinit();
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
                for (block.statements) |_stmt| {
                    try c.compile(.{ .statement = _stmt });
                }
            },

            .@"var" => |var_stmt| {
                // var [var_stmt.name :: identifier] = [var_stmt.value :: *expression]
                // var_stmt.value is the right side expression
                try c.compile(.{ .expression = var_stmt.value });
                //const identifier = var_stmt.name;
                const symbol = try c.symbols.define(var_stmt.name.value);
                try c.emit(.setgv, &.{symbol.index});
            },

            else => return error.InvalidStatemend,
        },

        .expression => |exp| switch (exp.*) {
            .identifier => |ident| {
                const symbol = c.symbols.resolve(ident.value) orelse return error.UndefinedVariable;
                try c.emit(.getgv, &.{symbol.index});
            },

            .infix => |infix| {
                const token = infix.token.type;

                if (token == .@"<") {
                    try c.compile(.{ .expression = infix.right });
                    try c.compile(.{ .expression = infix.left });
                    try c.emit(.gt, &.{});
                    return;
                }

                try c.compile(.{ .expression = infix.left });
                try c.compile(.{ .expression = infix.right });
                const op: code.Opcode = switch (token) {
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
                const token = prefix.token.type;

                try c.compile(.{ .expression = prefix.right });

                switch (token) {
                    .@"!" => try c.emit(.not, &.{}),
                    .@"-" => try c.emit(.min, &.{}),
                    else => unreachable,
                }
            },

            .integer => |int| {
                const pos = try c.addConstants(.{
                    .integer = .{ .value = int.value },
                });
                try c.emit(.constant, &.{pos});
            },

            .float => |float| {
                const pos = try c.addConstants(.{
                    .float = .{ .value = float.value },
                });
                try c.emit(.constant, &.{pos});
            },

            .string => |str| {
                const pos = try c.addConstants(.{
                    .string = .{ .value = str.value },
                });
                try c.emit(.constant, &.{pos});
            },

            .boolean => |boolean| {
                const op: code.Opcode = if (boolean.value) .true else .false;
                try c.emit(op, &.{});
            },

            .null => {
                try c.emit(.null, &.{});
            },

            .@"if" => |ifexp| {
                // AST if (condition) { consequence } else { alternative }
                //
                // compiling the condition
                try c.compile(.{ .expression = ifexp.condition });

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                try c.compile(.{ .statement = .{ .block = ifexp.consequence } });

                // statements add a pop in the end, wee drop the last pop (if return)
                c.ifLastInstructionIsPopcodeThenPopIt();

                // always jumb: null is returned
                const jum_pos = try c.emitPos(.jump, &.{9999});

                const after_consequence_position = c.insLen();
                // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                if (ifexp.alternative) |alt| {
                    try c.compile(.{ .statement = .{ .block = alt } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    c.ifLastInstructionIsPopcodeThenPopIt();
                } else {
                    try c.compile(.{ .expression = &NULL_EXP });
                }

                const after_alternative_pos = c.insLen();
                // replases the 9999 (.jump) to the correct operand (after_alternative_pos);
                try c.changeOperand(jum_pos, after_alternative_pos);

                // if (ifexp.alternative) |alt| {
                //     const jum_pos = try c.emitPos(.jump, &.{9999});
                //
                //     const after_consequence_position = c.insLen();
                //     // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                //     try c.changeOperand(jump_if_not_true_pos, after_consequence_position);
                //
                //     try c.compile(.{ .statement = .{ .block_statement = alt } });
                //
                //     // statements add a pop in the end, wee drop the last pop (if return)
                //     c.ifLastInstructionIsPopcodeThenPopIt();
                //
                //     const after_alternative_pos = c.insLen();
                //     // replases the 9999 (.jump) to the correct operand (after_alternative_pos);
                //     try c.changeOperand(jum_pos, after_alternative_pos);
                //
                //     return;
                // }
                // // after_alternative_pos - jump_if_not_true_pos gives the offset
                // const after_consequence_position = c.insLen();
                // // replases the 999 to the correct operand (after_consequence_position);
                // try c.changeOperand(jump_if_not_true_pos, after_consequence_position);
            },

            else => |e| {
                std.log.err("find an invalid/not handled Expression: {}\n", .{e});
                return error.InvalidExpression;
            },
        },
    }
}

fn insLen(c: *Compiler) usize {
    var len: usize = 0;
    for (c.instructions.items) |ins| len += ins.len;
    return len;
}

fn ifLastInstructionIsPopcodeThenPopIt(c: *Compiler) void {
    if (c.last_ins.opcode == .pop) {
        c.allocator.free(c.instructions.pop());
        c.last_ins = c.prev_ins;
    }
}

fn addConstants(c: *Compiler, obj: object.Object) !usize {
    try c.constants.append(obj);
    return c.constants.items.len - 1;
}

fn addInstruction(c: *Compiler, ins: code.Instructions) !usize {
    const pos_new_ins = c.instructions.items.len;
    try c.instructions.append(ins);
    return pos_new_ins;
}

fn replaceInstruction(c: *Compiler, pos: usize, new_ins: []u8) !void {
    c.allocator.free(c.instructions.orderedRemove(pos));
    try c.instructions.insert(pos, new_ins);
}

/// replaces the instruction
fn changeOperand(c: *Compiler, op_pos: usize, operand: usize) !void {
    const op: code.Opcode = @enumFromInt(c.instructions.items[op_pos][0]);
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
    c.prev_ins = c.last_ins;
    c.last_ins = .{ .opcode = op, .pos = pos };
}

pub fn bytecode(c: *Compiler) !Bytecode {
    const ins = try std.mem.concat(c.allocator, u8, c.instructions.items);
    return .{ .constants = c.constants.items, .instructions = ins };
}

/// compiler generated bytecode
pub const Bytecode = struct {
    constants: []object.Object,
    instructions: code.Instructions,

    pub fn deinit(b: *Bytecode, c: *const Compiler) void {
        c.allocator.free(b.instructions);
    }
};

const std = @import("std");
const ast = @import("ast.zig");
const code = @import("code.zig");
const object = @import("object.zig");

var NULL_EXP: ast.Expression = .null;

const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: usize,
};

/// symbles (identifier table)
/// Information such as its location, its scope, whether it was previously declared or not
const SymbolTable = struct {
    /// identifier name to scope map
    store: std.StringArrayHashMap(Symbol),
    def_number: usize,

    const Symbol = struct {
        name: []const u8,
        scope: Scope,
        index: usize,
    };

    const Scope = enum {
        global,
        local,
    };

    pub fn init(alloc: std.mem.Allocator) SymbolTable {
        return .{
            .store = .init(alloc),
            .def_number = 0,
        };
    }

    pub fn deinit(t: *SymbolTable) void {
        t.store.deinit();
    }

    pub fn define(t: *SymbolTable, name: []const u8) !Symbol {
        const symbol: Symbol = .{
            .name = name,
            .scope = .global,
            .index = t.def_number,
        };
        try t.store.put(name, symbol);
        t.def_number += 1;
        return symbol;
    }

    pub fn resolve(t: *SymbolTable, name: []const u8) ?*Symbol {
        return t.store.getPtr(name);
    }

    test define {
        const talloc = std.testing.allocator;

        var expected: std.StringHashMap(Symbol) = .init(talloc);
        defer expected.deinit();

        try expected.put("a", .{ .name = "a", .scope = .global, .index = 0 });
        try expected.put("b", .{ .name = "b", .scope = .global, .index = 1 });

        var global = SymbolTable.init(talloc);
        defer global.deinit();

        const a = try global.define("a");
        const aexp = expected.get("a").?;
        if (!std.meta.eql(a, aexp)) {
            return error.UnexpendedSymbol;
        }

        const b = try global.define("b");
        const bexp = expected.get("b").?;
        if (!std.meta.eql(b, bexp)) {
            return error.UnexpendedSymbol;
        }
    }

    test resolve {
        const talloc = std.testing.allocator;

        const expected: [2]Symbol = .{
            .{ .name = "a", .scope = .global, .index = 0 },
            .{ .name = "b", .scope = .global, .index = 1 },
        };

        var global = SymbolTable.init(talloc);
        defer global.deinit();

        _ = try global.define("a");
        _ = try global.define("b");

        for (expected) |sym| {
            const result = global.resolve(sym.name).?.*;
            if (!std.meta.eql(sym, result)) {
                return error.UnexpendedSymbol;
            }
        }
    }
};

test {
    _ = @import("compiler_test.zig");
    _ = SymbolTable;
}
