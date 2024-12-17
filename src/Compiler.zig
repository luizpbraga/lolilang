const std = @import("std");
const ast = @import("ast.zig");
const code = @import("code.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const memory = @import("memory.zig");

var NULL_EXP: ast.Expression = .null;

/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();
allocator: std.mem.Allocator,

/// constants pool
constants: std.ArrayList(Value),
symbols: SymbolTable,

scopes: std.ArrayList(Scope),
scope_index: usize,

const Scope = struct {
    instructions: std.ArrayList(code.Instructions),
    last_ins: EmittedInstruction,
    prev_ins: EmittedInstruction,

    test "Scope" {
        const talloc = std.testing.allocator;
        var compiler: Compiler = try .init(talloc);
        defer compiler.deinit();

        if (compiler.scope_index != 0) {
            std.log.err("Expect 0, found {}", .{compiler.scope_index});
            return error.WrongScopeIndex;
        }

        try compiler.emit(.mul, &.{});

        try compiler.enterScope();

        if (compiler.scope_index != 1) {
            std.log.err("Expect 1, found {}", .{compiler.scope_index});
            return error.WrongScopeIndex;
        }

        try compiler.emit(.sub, &.{});
    }
};

pub fn init(alloc: std.mem.Allocator) !Compiler {
    const main_scope: Scope = .{
        .instructions = .init(alloc),
        .last_ins = undefined,
        .prev_ins = undefined,
    };

    var scopes: std.ArrayList(Scope) = .init(alloc);
    try scopes.append(main_scope);

    return .{
        .allocator = alloc,
        .constants = .init(alloc),
        .symbols = .init(alloc),
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
    c.symbols.deinit();
}

fn enterScope(c: *Compiler) !void {
    try c.scopes.append(.{
        .prev_ins = undefined,
        .last_ins = undefined,
        .instructions = .init(c.allocator),
    });
    c.scope_index += 1;
}

fn leaveScope(c: *Compiler) !code.Instructions {
    c.scope_index -= 1;
    var last_scope = c.scopes.pop();
    defer {
        for (last_scope.instructions.items) |ins| c.allocator.free(ins);
        last_scope.instructions.deinit();
    }
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
                const symbol = c.symbols.resolve(ident.value) orelse {
                    std.log.err("-> {s}", .{ident.value});
                    return error.UndefinedVariable;
                };
                try c.emit(.getgv, &.{symbol.index});
            },

            // .method => |method| {
            //     const ident = method.method;
            //     const caller = method.caller;
            // },

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
                try c.emit(.index, &.{});
            },

            .integer => |int| {
                const pos = try c.addConstants(.{
                    .integer = int.value,
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
                const obj = try c.allocator.create(Object);
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

            .function => |func| {
                try c.enterScope();
                try c.compile(.{ .statement = .{ .block = func.body } });
                const instruction = try c.leaveScope();
                const obj = try c.allocator.create(Object);
                obj.type = .{ .function = instruction };
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

            // BUG: if (true) { var x = 0 } overflows sthe stack
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
fn ifLastInstructionIsPopcodeThenPopIt(c: *Compiler) void {
    if (c.scopes.items[c.scope_index].last_ins.opcode == .pop) {
        // const last = c.scopes.items[c.scope_index].last_ins;
        // for (last.pos..c.currentInstruction().items.len) |i| {
        //     c.allocator.free(c.currentInstruction().orderedRemove(i));
        // }
        c.allocator.free(c.currentInstruction().pop());
        c.scopes.items[c.scope_index].last_ins = c.scopes.items[c.scope_index].prev_ins;
    }
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

pub fn bytecode(c: *Compiler) !Bytecode {
    const ins = try std.mem.concat(c.allocator, u8, c.currentInstruction().items);
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
        scope: ScopeType,
        index: usize,
    };

    const ScopeType = enum {
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
