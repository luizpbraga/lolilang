const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const TokenType = @import("Token.zig").TokenType;

const talloc = std.testing.allocator;

test "Parse RETURN statements: Size" {
    const expected_value = struct { i64, bool, []const u8 }{ 5, true, "y" };

    const tests = [_]struct {
        input: []const u8,
    }{
        .{ .input = "return 5;" },
        .{ .input = "return true;" },
        .{ .input = "return y;" },
    };

    inline for (tests, expected_value) |x, k| {
        var lexer = Lexer.init(x.input);

        var p = try Parser.new(talloc, &lexer);
        defer p.deinit();

        var program = try p.parseProgram();

        try std.testing.expect(program.statements.items.len == 1);
        var stmt = program.statements.items[0];

        var val = stmt.return_statement.value.?;

        try testLiteralExpression(&val, k);
    }
}

test "Parse VAR statements" {
    const expected_value = struct {
        i64, // bool, []const u8,
    }{
        5, // true, "y"
    };

    const tests = [_]struct {
        input: []const u8,
        expected_indetifier: []const u8,
    }{
        .{ .input = "var x = 5;", .expected_indetifier = "x" },
        // .{ .input = "var x = true;", .expected_indetifier = "x" },
        // .{ .input = "var x = y;", .expected_indetifier = "x" },
    };

    inline for (tests, expected_value) |x, k| {
        var lexer = Lexer.init(x.input);

        var p = try Parser.new(talloc, &lexer);
        defer p.deinit();

        const program = try p.parseProgram();

        try std.testing.expect(program.statements.items.len == 1);
        var stmt = program.statements.items[0];

        var val = stmt.var_statement.value.?;

        try testLiteralExpression(&val, k);
    }
}
test "Token test" {
    const input =
        \\var x = 100 + if (!true) 5 else -10;
        \\-5;
        \\"ola mundo";
        \\+=
    ;

    const tokens = [_]TokenType{ .@"var", .identifier, .@"=", .int, .@"+", .@"if", .@"(", .@"!", .true, .@")", .int, .@"else", .@"-", .int, .@";", .@"-", .int, .@";", .string, .@";", .@"+=" };

    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();

    var i: usize = 0;
    while (tok.type != .eof) {
        try std.testing.expect(tok.type == tokens[i]);
        tok = lexer.nextToken();
        i += 1;
    }
}

fn testIdentifier(exp: *ast.Expression, value: anytype) !void {
    if (@TypeOf(value) == bool) {
        const ident = exp.boolean;

        if (!ident.value == value)
            return error.UnexpectedValue;
        return;
    }

    const ident = exp.identifier;

    if (!std.mem.eql(u8, ident.value, value)) {
        std.log.err("find {s} expected {s}\n", .{ ident.value, value });
        return error.UnexpectedValue;
    }

    if (!std.mem.eql(u8, ident.tokenLiteral(), value)) {
        std.log.err("find {s} expected {s}\n", .{ ident.tokenLiteral(), value });
        return error.UnexpectedValue;
    }
}

fn testIntegerLiteral(exp: *ast.Expression, value: i64) !void {
    const integer = exp.integer_literal;
    var buff: [10]u8 = undefined;
    const value_str = try std.fmt.bufPrint(&buff, "{d}", .{value});

    if (integer.value != value)
        return error.UnexpectedValue;

    if (!std.mem.eql(u8, integer.tokenLiteral(), value_str))
        return error.UnexpectedValue;
}

fn testLiteralExpression(exp: *ast.Expression, expected: anytype) !void {
    if (@TypeOf(expected) == i64 or @TypeOf(expected) == comptime_int) {
        try testIntegerLiteral(exp, expected);
    } else {
        try testIdentifier(exp, expected);
    }
}
