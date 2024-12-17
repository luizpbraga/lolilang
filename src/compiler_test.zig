const std = @import("std");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const talloc = std.testing.allocator;
const Value = @import("Object.zig").Value;

const CompilerTestCase = struct {
    input: []const u8,
    expected_constants: []const usize,
    expected_instructions: []const []u8,
};

test "Function Expression" {
    const tests: []const struct {
        input: []const u8,
        expected_constants: []const struct { usize, usize, []const []u8 },
        expected_instructions: []const []u8,
    } = &.{
        .{
            .input = "fn() { return 5 + 10;}",
            .expected_constants = &.{.{
                5,
                10,
                &.{
                    try code.makeBytecode(talloc, .constant, &.{0}),
                    try code.makeBytecode(talloc, .constant, &.{1}),
                    try code.makeBytecode(talloc, .add, &.{}),
                    try code.makeBytecode(talloc, .retv, &.{}),
                },
            }},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{2}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    defer for (tests) |t| {
        for (t.expected_instructions) |bytes| talloc.free(bytes);
        for (t.expected_constants) |constatnts| {
            const ins = constatnts[2];
            for (ins) |in| talloc.free(in);
        }
    };
    try runCompilerTest(talloc, tests);
}

test "Index Expression" {
    const tests: []const struct {
        input: []const u8,
        expected_constants: []const usize,
        expected_instructions: []const []u8,
    } = &.{
        .{
            .input = "[1, 2, 3][1 + 1]",
            .expected_constants = &.{ 1, 2, 3, 1, 1 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .constant, &.{2}),
                try code.makeBytecode(talloc, .array, &.{3}),
                try code.makeBytecode(talloc, .constant, &.{3}),
                try code.makeBytecode(talloc, .constant, &.{4}),
                try code.makeBytecode(talloc, .add, &.{}),
                try code.makeBytecode(talloc, .index, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);
    try runCompilerTest(talloc, tests);
}

test "Arrays" {
    const tests: []const struct {
        input: []const u8,
        expected_constants: []const usize,
        expected_instructions: []const []u8,
    } = &.{
        .{
            .input = "[]",
            .expected_constants = &.{},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .array, &.{0}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "[1, 2, 3]",
            .expected_constants = &.{ 1, 2, 3 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .constant, &.{2}),
                try code.makeBytecode(talloc, .array, &.{3}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .expected_constants = &.{ 1, 2, 3, 4, 5, 6 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .add, &.{}),
                try code.makeBytecode(talloc, .constant, &.{2}),
                try code.makeBytecode(talloc, .constant, &.{3}),
                try code.makeBytecode(talloc, .sub, &.{}),
                try code.makeBytecode(talloc, .constant, &.{4}),
                try code.makeBytecode(talloc, .constant, &.{5}),
                try code.makeBytecode(talloc, .mul, &.{}),
                try code.makeBytecode(talloc, .array, &.{3}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);
    try runCompilerTest(talloc, tests);
}

test "String Expression" {
    const tests: []const struct {
        input: []const u8,
        expected_constants: []const []const u8,
        expected_instructions: []const []u8,
    } = &.{
        .{
            .input = "\"loli\"",
            .expected_constants = &.{"loli"},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },

        .{
            .input = "\"lo\" + \"li\"",
            .expected_constants = &.{ "lo", "li" },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .add, &.{}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };

    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);
    try runCompilerTest(talloc, tests);
}

test "Global Var Statement" {
    const tests: []const CompilerTestCase = &.{
        .{
            .input = "var one = 1; var two = 2;",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .setgv, &.{0}),
                try code.makeBytecode(talloc, .constant, &.{1}),
                try code.makeBytecode(talloc, .setgv, &.{1}),
            },
        },
        .{
            .input = "var one = 1; one;",
            .expected_constants = &.{1},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .setgv, &.{0}),
                try code.makeBytecode(talloc, .getgv, &.{0}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "var one = 1; var two = one; two;",
            .expected_constants = &.{1},
            .expected_instructions = &.{
                try code.makeBytecode(talloc, .constant, &.{0}),
                try code.makeBytecode(talloc, .setgv, &.{0}),
                try code.makeBytecode(talloc, .getgv, &.{0}), // ?
                try code.makeBytecode(talloc, .setgv, &.{1}),
                try code.makeBytecode(talloc, .getgv, &.{1}),
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };
    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);
    try runCompilerTest(talloc, tests);
}

test "Conditionals" {
    const tests: []const CompilerTestCase = &.{
        .{
            .input = "if (true) { 10 } else { 20 }; 666;",
            .expected_constants = &.{ 10, 20, 666 },
            .expected_instructions = &.{
                // 0000
                try code.makeBytecode(talloc, .true, &.{}),
                // 0001
                try code.makeBytecode(talloc, .jumpifnottrue, &.{10}),
                // 0004
                try code.makeBytecode(talloc, .constant, &.{0}),
                // 0007
                try code.makeBytecode(talloc, .jump, &.{13}),
                // 00010
                try code.makeBytecode(talloc, .constant, &.{1}),
                // 0013
                try code.makeBytecode(talloc, .pop, &.{}),
                //0014
                try code.makeBytecode(talloc, .constant, &.{2}),
                // 0017
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
        .{
            .input = "if (true) { 10 }; 666;",
            .expected_constants = &.{ 10, 666 },
            .expected_instructions = &.{
                // 0000
                try code.makeBytecode(talloc, .true, &.{}),
                // 0001
                try code.makeBytecode(talloc, .jumpifnottrue, &.{10}),
                // 0004
                try code.makeBytecode(talloc, .constant, &.{0}),
                // 0007
                try code.makeBytecode(talloc, .jump, &.{11}),
                // 0008
                try code.makeBytecode(talloc, .null, &.{}),
                // 0011
                try code.makeBytecode(talloc, .pop, &.{}),
                // 0012
                try code.makeBytecode(talloc, .constant, &.{1}),
                // 0015
                try code.makeBytecode(talloc, .pop, &.{}),
            },
        },
    };
    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);
    try runCompilerTest(talloc, tests);
}

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
    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);

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
    defer for (tests) |t| for (t.expected_instructions) |bytes| talloc.free(bytes);

    try runCompilerTest(talloc, tests);
}

fn runCompilerTest(alloc: anytype, tests: anytype) !void {
    for (tests) |t| {
        // defer for (t.expected_instructions) |bytes| {
        //     alloc.free(bytes);
        // };

        var lexer = Lexer.init(t.input);
        var parser = Parser.new(alloc, &lexer);
        defer parser.deinit();

        const node = try parser.parse();

        const Vm = @import("Vm.zig");

        var compiler = try Compiler.init(alloc);
        defer compiler.deinit();

        try compiler.compile(node);
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        var vm: Vm = try .init(alloc, &b);
        defer vm.deinit();

        try checkInstructions(alloc, t.expected_instructions, b.instructions);
        try checkConstants(t.expected_constants, b.constants);
    }
}

fn checkInstructions(alloc: anytype, expected: []const code.Instructions, actual: code.Instructions) !void {
    const concatted = try std.mem.concat(alloc, u8, expected);
    defer alloc.free(concatted);

    if (actual.len != concatted.len) {
        try logBytecode(alloc, concatted, actual);
        return error.WrongInstructionLenght;
    }

    for (actual, concatted) |act, ins| {
        if (act != ins) {
            try logBytecode(alloc, concatted, actual);
            return error.WrongInstruction;
        }
    }
}

fn logBytecode(alloc: anytype, concatted: []u8, actual: []u8) !void {
    const exp_fmt = try code.formatInstruction(alloc, concatted);
    defer alloc.free(exp_fmt);
    const act_fmt = try code.formatInstruction(alloc, actual);
    defer alloc.free(act_fmt);
    std.log.err("\nExpected:'{s}'\nGot:'{s}'\n", .{ exp_fmt, act_fmt });
}

fn checkConstants(expected: anytype, actual: []Value) !void {
    if (expected.len != actual.len) return error.WrongNumberOfConstants;

    for (actual, expected) |act, con| {
        switch (@typeInfo(@TypeOf(expected)).pointer.child) {
            usize, i32, i64 => {
                try checkIntegerObject(@intCast(con), act);
            },
            []u8, []const u8 => {
                try checkStringObject(con, act);
            },

            // function test
            struct { usize, usize, []const []u8 } => {},
            else => {},
        }
    }
}

fn checkStringObject(exp: []const u8, act: Value) !void {
    const result = switch (act) {
        .obj => |ob| switch (ob.type) {
            .string => |i| i,
            else => return error.NotAString,
        },
        else => return error.NotAString,
    };

    if (!std.mem.eql(u8, result, exp)) {
        std.log.err("Expect: {s} Got: {s}\n", .{ exp, result });
        return error.WrongStringValue;
    }
}

fn checkIntegerObject(exp: i64, act: Value) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAInteger,
    };

    if (result != exp) {
        std.log.err("Expect: {} Got: {}\n", .{ exp, result });
        return error.WrongIntegerValue;
    }
}
