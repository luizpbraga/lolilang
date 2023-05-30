const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const TokenType = @import("Token.zig").TokenType;

// TODO: implement helper function e.g. testInfixExpression, testIdentifier, testBooleanLiteral...

test "If Expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("if (x < y) {x}");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;
    var exp = stmt.expression.?.if_expression;
    _ = exp;
}

test "Group Exp" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("((-(5 + 5) * 5) == 10) != !true");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    // var stmt = program.statements.items[0].expression_statement;
    // var exp = stmt.expression.?;
    // std.debug.print("{}", .{exp});
}

test "eval Boolean" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("false");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;
    var exp = stmt.expression.?;

    try std.testing.expect(false == exp.boolean.value);
}

test "Boolean" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        l: bool,
        r: bool,
        op: []const u8,
        input: []const u8,
    }{
        .{ .input = "true == true", .l = true, .r = true, .op = "==" },
        .{ .input = "false == false", .l = false, .r = false, .op = "==" },
        .{ .input = "false != true", .l = false, .r = true, .op = "!=" },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        var stmt = program.statements.items[0].expression_statement;

        var exp = stmt.expression.?.infix_expression;

        const operator = exp.operator;

        if (!std.mem.eql(u8, operator, x.op))
            return error.UnexpectedOP;

        const left = exp.left.?.boolean;
        const right = exp.right.?.boolean;

        if (left.value != x.l or right.value != x.r)
            return error.UnexpectedValue;
    }
}

test "Parse Infix OP " {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        l: usize,
        r: usize,
        op: []const u8,
        input: []const u8,
    }{
        .{ .input = "10 + 5", .l = 10, .r = 5, .op = "+" },
        .{ .input = "10 - 5", .l = 10, .r = 5, .op = "-" },
        .{ .input = "10 * 5", .l = 10, .r = 5, .op = "*" },
        .{ .input = "10 / 5", .l = 10, .r = 5, .op = "/" },
        .{ .input = "10 > 5", .l = 10, .r = 5, .op = ">" },
        .{ .input = "10 < 5", .l = 10, .r = 5, .op = "<" },
        .{ .input = "10 == 5", .l = 10, .r = 5, .op = "==" },
        .{ .input = "10 != 5", .l = 10, .r = 5, .op = "!=" },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        var stmt = program.statements.items[0].expression_statement;

        var exp = stmt.expression.?.infix_expression;

        const operator = exp.operator;

        if (!std.mem.eql(u8, operator, x.op))
            return error.UnexpectedOP;

        const left = exp.left.?.integer_literal;
        const right = exp.right.?.integer_literal;

        if (left.value != x.l or right.value != x.r)
            return error.UnexpectedValue;
    }
}

test "Parse Prefix OP (!)" {
    const allocator = std.testing.allocator;
    const input = "!5;";
    const output = 5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;

    var exp = stmt.expression.?.prefix_expression;

    if (.@"!" != exp.token.type) {
        return error.UnexpecedOperator;
    }

    var integer = exp.right.?.integer_literal;

    if (output != integer.value) {
        return error.UnexpectedValue;
    }
}

test "parse Prefix OP (-)" {
    const allocator = std.testing.allocator;
    const input = "-5;";
    const output = -5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    // for (program.statements.items) |item| {
    //     //     std.debug.print("{}\n\n", .{item.expression_statement.expression.?});
    //     std.debug.print("\n\n{}\n\n", .{item.expression_statement.expression.?.prefix_expression.right.?});
    // }

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;

    var exp = stmt.expression.?.prefix_expression;

    if (.@"-" != exp.token.type) {
        return error.UnexpecedOperator;
    }

    var integer = exp.right.?.integer_literal;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }
}

test "Eval Integer Literal Expression" {
    const allocator = std.testing.allocator;
    const input = "5;";

    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0];

    if (stmt != .expression_statement) {
        return error.ExprectAnExpression;
    }

    const literal = stmt.expression_statement.expression.?.integer_literal;

    if (5 != literal.value) {
        return error.UnexpectedValue;
    }

    if (!std.mem.eql(u8, "5", literal.tokenLiteral())) {
        return error.UnexpectedValue;
    }
}

test "eval Expression" {
    const allocator = std.testing.allocator;
    const input = "foobar;";

    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0];

    if (stmt != .expression_statement) {
        return error.ExprectAnExpression;
    }

    const ident = stmt.expression_statement.expression.?.identifier;
    const value = ident.value;

    if (!std.mem.eql(u8, "foobar", value)) {
        return error.UnexpectedValue;
    }

    if (!std.mem.eql(u8, "foobar", ident.tokenLiteral())) {
        return error.UnexpectedValue;
    }
}

test "eval Program" {
    const allocator = std.testing.allocator;
    var stmts = std.ArrayList(ast.Statement).init(allocator);
    defer stmts.deinit();

    var stmt = ast.Statement{
        .var_statement = .{
            .token = .{
                .type = .@"var",
                .literal = "var",
            },
            .name = .{
                .token = .{
                    .type = .identifier,
                    .literal = "myVar",
                },
                .value = "myVar",
            },
            .value = .{
                .identifier = .{
                    .token = .{
                        .type = .identifier,
                        .literal = "anotherVar",
                    },
                    .value = "anotherVar",
                },
            },
        },
    };

    try stmts.append(stmt);

    var program = ast.Program{ .statements = stmts };
    _ = program;
    // std.debug.print("{s}", .{program.tokenLiteral()});
    // try std.testing.expect( std.mem.eql(u8, "var myVar = anotherVar;", program.tokenLiteral()) );
}

test "Parse RETURN statements: Size" {
    const allocator = std.testing.allocator;
    const input =
        \\return x;
        \\return 10;
        \\return 9999;
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();
    var program = try parser.parseProgram(allocator);
    defer program.statements.deinit();
    try std.testing.expect(program.statements.items.len == 3);
}

test "Parse VAR statements: Size" {
    const allocator = std.testing.allocator;
    const input =
        \\var x = 5;
        \\var t = 10;
        \\var foo = 9999;
    ;

    var lexer = Lexer.init(input);
    var p = Parser.new(allocator, &lexer);
    defer p.deinit();
    var program = try p.parseProgram(allocator);
    defer program.statements.deinit();
    try std.testing.expect(program.statements.items.len == 3);
}

test "Token test" {
    const input =
        \\var x = 100 + if (!true) 5 else -10;
        \\-5;
    ;

    const tokens = [_]TokenType{
        .@"var",
        .identifier,
        .@"=",
        .int,
        .@"+",
        .@"if",
        .@"(",
        .@"!",
        .true,
        .@")",
        .int,
        .@"else",
        .@"-",
        .int,
        .@";",
        .@"-",
        .int,
        .@";",
    };

    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();

    var i: usize = 0;
    while (tok.type != .eof) {
        try std.testing.expect(tok.type == tokens[i]);
        tok = lexer.nextToken();
        i += 1;
    }
}
