/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();
/// constants pool
allocator: std.mem.Allocator,
constants: std.ArrayList(object.Object),
instructions: std.ArrayList(code.Instructions),
last_ins: EmittedInstruction,
prev_ins: EmittedInstruction,

const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: usize,
};

pub fn init(alloc: anytype) Compiler {
    return .{
        .allocator = alloc,
        .constants = .init(alloc),
        .instructions = .init(alloc),
        .last_ins = undefined,
        .prev_ins = undefined,
    };
}

pub fn deinit(c: *Compiler) void {
    // std.debug.print("{x}\n", .{c.instructions.items});
    for (c.instructions.items) |ins| c.allocator.free(ins);
    c.constants.deinit();
    c.instructions.deinit();
}

/// Walks the AST recursively and evaluate the node, and add it the the pool
pub fn compile(c: *Compiler, node: ast.Node) !void {
    switch (node) {
        .statement => |stmt| switch (stmt) {
            .program_statement => |program| {
                for (program.statements.items) |s| {
                    try c.compile(.{ .statement = s });
                }
            },

            .expression_statement => |exp_stmt| {
                try c.compile(.{ .expression = exp_stmt.expression });
                try c.emit(.pop, &.{});
            },

            .block_statement => |block| {
                for (block.statements) |_stmt| {
                    try c.compile(.{ .statement = _stmt });
                }
            },

            else => return error.InvalidStatemend,
        },

        .expression => |exp| switch (exp.*) {
            .infix_expression => |infix| {
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

            .prefix_expression => |prefix| {
                const token = prefix.token.type;

                try c.compile(.{ .expression = prefix.right });

                switch (token) {
                    .@"!" => try c.emit(.not, &.{}),
                    .@"-" => try c.emit(.min, &.{}),
                    else => return error.UnknowOperator,
                }
            },

            .integer_literal => |int| {
                const pos = try c.addConstants(.{
                    .integer = .{ .value = int.value },
                });
                try c.emit(.constant, &.{pos});
            },

            .boolean => |boolean| {
                const op: code.Opcode = if (boolean.value) .true else .false;
                try c.emit(op, &.{});
            },

            .if_expression => |ifexp| {
                // AST if (condition) { consequence } else { alternative }
                //
                // compiling the condition
                try c.compile(.{ .expression = ifexp.condition });

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                // compiling the consequence
                try c.compile(.{ .statement = .{ .block_statement = ifexp.consequence } });

                // statements add a pop in the end, wee drop the last pop (if return)
                c.ifLastInstructionIsPopcodeThenPopIt();

                if (ifexp.alternative) |alt| {
                    const jum_pos = try c.emitPos(.jump, &.{9999});

                    const after_consequence_position = c.insLen();
                    // replases the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                    try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

                    try c.compile(.{ .statement = .{ .block_statement = alt } });

                    // statements add a pop in the end, wee drop the last pop (if return)
                    c.ifLastInstructionIsPopcodeThenPopIt();

                    const after_alternative_pos = c.insLen();
                    // replases the 9999 (.jump) to the correct operand (after_alternative_pos);
                    try c.changeOperand(jum_pos, after_alternative_pos);

                    return;
                }

                // after_alternative_pos - jump_if_not_true_pos gives the offset
                const after_consequence_position = c.insLen();
                // replases the 999 to the correct operand (after_consequence_position);
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);
            },

            else => return error.InvalidExpression,
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
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const talloc = std.testing.allocator;

const CompilerTestCase = struct {
    input: []const u8,
    expected_constants: []const usize,
    expected_instructions: []const []u8,
};

test "Integer Arithmetic" {
    const tests: []const CompilerTestCase = &.{
        .{
            .input = "-1",
            .expected_constants = &.{1},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .min, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 + 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .add, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 - 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .sub, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 * 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .mul, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 / 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .div, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1;2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .pop, &.{}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    try runCompilerTest(talloc, tests);
}

test "Boolean Expression" {
    const tests: []const CompilerTestCase = &.{
        .{
            .input = "!true",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .true, &.{}),
                try code.makeBytecode(talloc, .not, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "true",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .true, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .false, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 > 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .gt, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 < 2",
            .expected_constants = &.{ 2, 1 }, // ATTENTION!!
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .gt, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 != 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .neq, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "1 == 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .eq, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },

        .{
            .input = "true != false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .true, &.{}),
                try code.makeBytecode(talloc, .false, &.{}),
                try code.makeBytecode(talloc, .neq, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "true == false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .true, &.{}),
                try code.makeBytecode(talloc, .false, &.{}),
                try code.makeBytecode(talloc, .eq, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    try runCompilerTest(talloc, tests);
}

fn runCompilerTest(alloc: anytype, tests: anytype) !void {
    for (tests) |t| {
        defer for (t.expected_instructions) |bytes| {
            alloc.free(bytes);
        };

        var lexer = Lexer.init(t.input);
        var parser = Parser.new(alloc, &lexer);
        defer parser.deinit();
        const program = try parser.parseProgram();

        var compiler = Compiler.init(alloc);
        defer compiler.deinit();
        try compiler.compile(.{
            .statement = .{ .program_statement = program },
        });
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        try checkInstructions(alloc, t.expected_instructions, b.instructions);
        try checkConstants(t.expected_constants, b.constants);
    }
}

fn checkInstructions(alloc: anytype, expected: []const code.Instructions, actual: code.Instructions) !void {
    const concatted = try std.mem.concat(alloc, u8, expected);
    defer alloc.free(concatted);

    if (actual.len != concatted.len) {
        std.log.err("want='{s}'\ngot='{s}'\n", .{ actual, concatted });
        return error.WrongInstructionLenght;
    }

    for (actual, concatted) |act, ins| {
        if (act != ins) return error.WrongInstruction;
    }
}

fn checkConstants(expected: anytype, actual: []object.Object) !void {
    if (expected.len != actual.len) return error.WrongNumberOfConstants;

    for (actual, expected) |act, con| {
        switch (@typeInfo(@TypeOf(expected)).pointer.child) {
            usize, i32, i64 => {
                try checkIntegerObject(@intCast(con), act);
            },
            else => {},
        }
    }
}

fn checkIntegerObject(exp: i64, act: object.Object) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAInteger,
    };

    if (result.value != exp) {
        std.log.err("Expect: {} Got: {}\n", .{ exp, result.value });
        return error.WrongIntegerValue;
    }
}

test {
    _ = @import("compiler_test.zig");
}
