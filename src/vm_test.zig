const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const talloc = std.testing.allocator;

test "Index" {
    const tests: []const struct {
        input: []const u8,
        expected: []const usize,
    } = &.{
        .{ .input = "[1][0]", .expected = &.{1} },
        .{ .input = "[1, 2][1]", .expected = &.{2} },
        .{ .input = "[0, 1, 2 + 2][2]", .expected = &.{4} },
    };

    try runVmTests(talloc, tests);
}

test "Array" {
    const tests: []const struct {
        input: []const u8,
        expected: []const usize,
    } = &.{
        .{ .input = "[]", .expected = &.{} },
        .{ .input = "[1, 2]", .expected = &.{ 1, 2 } },
        .{ .input = "[1, 2 + 2]", .expected = &.{ 1, 4 } },
    };

    try runVmTests(talloc, tests);
}

test "String Expression" {
    const tests: []const struct {
        input: []const u8,
        expected: []const u8,
    } = &.{
        .{ .input = 
        \\"lolilang"
        , .expected = "lolilang" },

        .{ .input = 
        \\"loli" + "lang"
        , .expected = "lolilang" },
    };

    try runVmTests(talloc, tests);
}

test "Globals Var Statements" {
    const tests: []const struct {
        input: []const u8,
        expected: usize,
    } = &.{
        .{ .input = "var one = 1; one", .expected = 1 },
        .{ .input = "var one = 1; var two = 2; one + two", .expected = 3 },
        .{ .input = "var one = 10; var two = one + one; one + two", .expected = 30 },
        .{ .input = 
        \\var one = 10
        \\var two = one + one 
        \\var z = if true { one + two } else { 0 }
        \\z + one + two
        , .expected = 30 + 10 + 10 + 10 },
    };

    try runVmTests(talloc, tests);
}

test "Conditional If" {
    const tests1: []const struct {
        input: []const u8,
        expected: isize,
    } = &.{
        .{ .input = "if true { -1 }", .expected = -1 },
        .{ .input = "if true { -1 } else { 2 }", .expected = -1 },
        .{ .input = "if false { -1 } else { 2 }", .expected = 2 },
        .{ .input = "if (1 < 2) { -1 } else { 2 }", .expected = -1 },
        .{ .input = "if (!!(if (false) { 10 })) { 10 } else { 20 }", .expected = 20 },
    };

    try runVmTests(talloc, tests1);

    const tests2: []const struct {
        input: []const u8,
        expected: object.Object,
    } = &.{
        .{ .input = "if false { -1 }", .expected = object.NULL },
        .{ .input = "if (1 > 2) { -1 }", .expected = object.NULL },
    };

    try runVmTests(talloc, tests2);
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

        const node = try parser.parse();

        var compiler: Compiler = .init(alloc);
        defer compiler.deinit();

        try compiler.compile(node);
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        // DEBUG
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

fn checkStringObject(exp: []const u8, act: object.Object) !void {
    const result = switch (act) {
        .string => |i| i,
        else => return error.NotAString,
    };

    if (!std.mem.eql(u8, result.value, exp)) return error.WrongStringValue;
}

fn checkArrayObject(exp: []const usize, act: object.Object) !void {
    const result = switch (act) {
        .array => |i| i,
        else => return error.NotAArray,
    };

    if (exp.len != result.elements.len) {
        return error.WrongArrayLenght;
    }

    for (exp, result.elements) |e, element|
        try try checkIntegerObject(e, element);
}

fn expectedObject(expected: anytype, actual: object.Object) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int => try checkIntegerObject(@intCast(expected), actual),
        .bool => try checkBooleanObject(expected, actual),
        .null => if (actual.null.value != null) {
            return error.ExpectNullObject;
        },
        .array => try checkArrayObject(expected, actual),

        else => {},
    }
}
