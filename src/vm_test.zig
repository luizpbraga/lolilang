const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const talloc = std.testing.allocator;

test "Local Binding" {
    const tests: []const struct {
        input: []const u8,
        expected: i64,
    } = &.{
        .{ .input = "var ten = fn(){ var ten = 10; return ten }; ten()", .expected = 10 },
    };

    try runVmTests(talloc, tests);
}

test "Function Without Arguments" {
    {
        const tests: []const struct {
            input: []const u8,
            expected: i64,
        } = &.{
            .{ .input = "var foo = fn(){ 10 }; foo()", .expected = 10 },
            .{ .input = "var foo = fn(){ 10 }; var bar = fn(){ foo() + 20 }; bar()", .expected = 30 },
            .{ .input = "var foo = fn(){ 10 }; var bar = fn(){ foo() + 20 }; foo() + bar()", .expected = 40 },
            .{ .input = "var foo = fn(){ return 40; 10 }; foo()", .expected = 40 },
            .{ .input = "var foo = fn(){ return 40; 10 }; var bar = fn(){ foo }; bar()()", .expected = 40 },
        };

        try runVmTests(talloc, tests);
    }

    {
        const tests: []const struct {
            input: []const u8,
            expected: Value,
        } = &.{
            .{ .input = "var foo = fn(){ }; foo()", .expected = .null },
            .{ .input = "var foo = fn(){ return null }; foo()", .expected = .null },
        };

        try runVmTests(talloc, tests);
    }
}

test "Index" {
    const tests: []const struct {
        input: []const u8,
        expected: i64,
    } = &.{
        .{ .input = "[1][0]", .expected = 1 },
        .{ .input = "[1, 2][1]", .expected = 2 },
        .{ .input = "[0, 1, 2 + 2][2]", .expected = 4 },
    };

    try runVmTests(talloc, tests);
}

test "Array" {
    const tests: []const struct {
        input: []const u8,
        expected: []const i64,
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
        // .{ .input = "if (!!(if (false) { 10 })) { 10 } else { 20 }", .expected = 20 },
    };

    try runVmTests(talloc, tests1);

    const tests2: []const struct {
        input: []const u8,
        expected: Value,
    } = &.{
        .{ .input = "if false { -1 }", .expected = .null },
        .{ .input = "if (1 > 2) { -1 }", .expected = .null },
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

        var compiler: Compiler = try .init(alloc);
        defer compiler.deinit();

        try compiler.compile(node);
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        // DEBUG
        // const fmt = try code.formatInstruction(alloc, b.instructions);
        // defer alloc.free(fmt);
        // std.debug.print("input:'{s}'\nfmt:'{s}'\n", .{ t.input, fmt });

        var vm: Vm = try .init(alloc, &b);
        defer vm.deinit();

        try vm.run();

        const obj = vm.lastPopped();
        try expectedObject(t.expected, obj);
    }
}

fn checkIntegerObject(exp: i64, act: Value) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAnInteger,
    };

    if (result != exp) return error.WrongIntegerValue;
}

fn checkBooleanObject(exp: bool, act: Value) !void {
    const result = switch (act) {
        .boolean => |i| i,
        else => {
            std.debug.print("{}", .{act});
            return error.NotABoolean;
        },
    };

    if (result != exp) return error.WrongBooleanValue;
}

fn checkStringObject(exp: []const u8, act: Value) !void {
    const result = switch (act) {
        .obj => |ob| switch (ob.type) {
            .string => |str| str,
            else => return error.NotAString,
        },
        else => return error.NotAString,
    };

    if (!std.mem.eql(u8, result, exp)) return error.WrongStringValue;
}

fn checkArrayObject(exp: []const i64, act: Value) !void {
    const result = switch (act) {
        .obj => |ob| switch (ob.type) {
            .array => |arr| arr,
            else => return error.NotAArray,
        },
        else => return error.NotAArray,
    };

    if (exp.len != result.len) {
        return error.WrongArrayLenght;
    }

    for (exp, result) |e, element|
        try checkIntegerObject(e, element);
}

fn expectedObject(expected: anytype, actual: Value) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int => try checkIntegerObject(@intCast(expected), actual),
        .bool => try checkBooleanObject(expected, actual),
        .null => if (actual.null != null) {
            return error.ExpectNullObject;
        },

        .pointer => |p| {
            switch (p.child) {
                i64 => try checkArrayObject(expected, actual),
                u8 => try checkStringObject(expected, actual),
                else => return error.invalidpointertype,
            }
        },

        .@"union" => {
            if (@TypeOf(expected) == Value) {
                try std.testing.expectEqual(actual, expected);
            }
        },

        else => {
            std.debug.print("find type = {}", .{@TypeOf(expected)});
            return error.UnkowType;
        },
    }
}
