const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const talloc = std.testing.allocator;

test "Conditionals" {
    const tests: []const struct {
        input: []const u8,
        expected: isize,
    } = &.{
        .{ .input = "if true { -1 }", .expected = -1 },
        .{ .input = "if true { -1 } else { 2 }", .expected = -1 },
        .{ .input = "if false { -1 } else { 2 }", .expected = 2 },
        .{ .input = "if (1 < 2) { -1 } else { 2 }", .expected = -1 },
    };

    try runVmTests(talloc, tests);
}

test "Integer Arithmetic" {
    const tests: []const struct {
        input: []const u8,
        expected: isize,
    } = &.{
        .{ .input = "-1", .expected = -1 },
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 2", .expected = 3 },
        .{ .input = "1 - 2", .expected = -1 },
        .{ .input = "1 * 2", .expected = 2 },
        .{ .input = "2 - 1", .expected = 1 },
        .{ .input = "2 - 1 * 2", .expected = 0 },
        .{ .input = "(2 - 1) * 2", .expected = 2 },
        .{ .input = "50 / 2 * 2 + 10 - 5", .expected = 55 },
    };

    try runVmTests(talloc, tests);
}

test "Boolean Expression" {
    const tests: []const struct {
        input: []const u8,
        expected: bool,
    } = &.{
        .{ .input = "!true", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "-1 == -1", .expected = true },
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "false == false", .expected = true },
        .{ .input = "false == true", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 > 2) != true", .expected = true },
        .{ .input = "(1 < 2) != true", .expected = false },
    };

    try runVmTests(talloc, tests);
}

fn runVmTests(alloc: anytype, tests: anytype) !void {
    for (tests) |t| {
        var lexer: Lexer = .init(t.input);
        var parser: Parser = .new(alloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var compiler: Compiler = .init(alloc);
        defer compiler.deinit();

        try compiler.compile(.{ .statement = .{ .program_statement = program } });
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        // const fmt = try code.formatInstruction(alloc, b.instructions);
        // defer alloc.free(fmt);
        // std.debug.print("input:'{s}'\nfmt:'{s}'\n", .{ t.input, fmt });

        const obj = try Vm.runVm(&b);
        try expectedObject(t.expected, obj);
    }
}

fn checkIntegerObject(exp: i64, act: object.Object) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAnInteger,
    };

    if (result.value != exp) return error.WrongIntegerValue;
}

fn checkBooleanObject(exp: bool, act: object.Object) !void {
    const result = switch (act) {
        .boolean => |i| i,
        else => return error.NotABoolean,
    };

    if (result.value != exp) return error.WrongBooleanValue;
}

fn expectedObject(expected: anytype, actual: object.Object) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int => try checkIntegerObject(@intCast(expected), actual),
        .bool => try checkBooleanObject(expected, actual),
        else => {},
    }
}
