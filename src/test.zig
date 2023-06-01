const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const TokenType = @import("Token.zig").TokenType;

// TODO: implement helper function e.g. testInfixExpression, testIdentifier, testBooleanLiteral...
//
fn testIdentifier(exp: *ast.Expression, value: []const u8) !void {
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
    if (@TypeOf(expected) == i64) {
        try testIntegerLiteral(exp, expected);
    } else {
        try testIdentifier(exp, expected);
    }
}

fn testInfixExpression(exp: *ast.Expression, left: anytype, op: []const u8, right: anytype) !void {
    var opExp = exp.infix_expression;

    try testLiteralExpression(opExp.left, left);

    if (!std.mem.eql(u8, op, opExp.operator))
        return error.UnexpectedOP;

    try testLiteralExpression(opExp.right, right);
}

test "Function Call" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("add()");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }
}

test "Function Literal" {
    const allocator = std.testing.allocator;

    const tests = [_]struct {
        input: []const u8,
        len: usize,
    }{
        // .{ .input = "fn(){};", .len = 0 },
        .{ .input = "fn(x, y){ x + y; };", .len = 2 },
        // .{ .input = "fn(x, y, z){};", .len = 3 },
        // .{ .input = "fn(x, y, z){ var x = 10; };", .len = 3 },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        var stmt = program.statements.items[0].expression_statement;

        var func = stmt.expression.function_literal;

        if (func.parameters.len != x.len) {
            std.log.err("\n{s}\n", .{x.input});
            return error.WrongParmeterLengh;
        }

        if (!std.mem.eql(u8, func.parameters[0].value, "x"))
            return error.UnexpectedValue;

        if (func.body.statements.len != 1)
            return error.WrongFunctionBody;

        var body_stmt = func.body.statements[0].expression_statement;

        try testInfixExpression(body_stmt.expression, "x", "+", "y");
    }
}

// test "var x = (((( 10 ))))" {
//     const allocator = std.testing.allocator;
//     var lexer = Lexer.init("var x = 10;");
//     var parser =try Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram(allocator);
//     defer program.statements.deinit();

//     if (program.statements.items.len != 1) {
//         std.log.err("len: {d}", .{program.statements.items.len});
//         return error.NotEnoughStatements;
//     }

//     var stmt = program.statements.items[0].var_statement;
//     std.debug.print("\n{s}\n", .{stmt.tokenLiteral()});
//     std.debug.print("\n{s} {s}\n", .{ stmt.tokenLiteral(), stmt.name.value });
// }

test "If Else Expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(
        \\\\nenem
        \\if (x < y) {x} else {y}
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;
    var exp = stmt.expression.if_expression;

    try testInfixExpression(exp.condition, "x", "<", "y");

    if (exp.consequence.statements.len != 1)
        return error.ConsequenceIs1Statement;

    var consequence = exp.consequence.statements[0].expression_statement;

    try testIdentifier(consequence.expression, "x");

    if (exp.alternative == null)
        return error.AlternativeStatementsWasNotNull;

    var alternative = exp.alternative.?.statements[0].expression_statement;
    try testIdentifier(alternative.expression, "y");
}

test "If Expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("if (x < y) {x}");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;
    var exp = stmt.expression.if_expression;

    try testInfixExpression(exp.condition, "x", "<", "y");

    if (exp.consequence.statements.len != 1)
        return error.ConsequenceIs1Statement;

    var consequence = exp.consequence.statements[0].expression_statement;
    _ = consequence;

    if (exp.alternative != null)
        return error.AlternativeStatementsWasNotNull;
}

test "Group Exp" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("((-(5 + 5) * 5) == 10) != !true");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    // var stmt = program.statements.items[0].expression_statement;
    // var exp = stmt.expression;
    // std.debug.print("{}", .{exp});
}

test "eval Boolean" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("false");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;
    var exp = stmt.expression;

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
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        var stmt = program.statements.items[0].expression_statement;

        var exp = stmt.expression.infix_expression;

        const operator = exp.operator;

        if (!std.mem.eql(u8, operator, x.op))
            return error.UnexpectedOP;

        const left = exp.left.boolean;
        const right = exp.right.boolean;

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
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        var stmt = program.statements.items[0].expression_statement;

        var exp = stmt.expression.infix_expression;

        const operator = exp.operator;

        if (!std.mem.eql(u8, operator, x.op))
            return error.UnexpectedOP;

        const left = exp.left.integer_literal;
        const right = exp.right.integer_literal;

        if (left.value != x.l or right.value != x.r)
            return error.UnexpectedValue;
    }
}

test "Parse Prefix OP (!)" {
    const allocator = std.testing.allocator;
    const input = "!5;";
    const output = 5;

    var lexer = Lexer.init(input);
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;

    var exp = stmt.expression.prefix_expression;

    if (.@"!" != exp.token.type) {
        return error.UnexpecedOperator;
    }

    var integer = exp.right.integer_literal;

    if (output != integer.value) {
        return error.UnexpectedValue;
    }
}

test "Comment" {
    const allocator = std.testing.allocator;
    // const input =
    //     \\\\ +-----------------------------+
    //     \\\\ isso é um comentário
    //     \\\\ isso tambem é um comentário
    //     \\\\ +-----------------------------+
    //     \\-5;
    //     \\\\ AAAAAAAAAAAAI PAPY
    // ;
    //

    const input = @embedFile("./main.lol");

    const output = -5;

    var lexer = Lexer.init(input);
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;

    var exp = stmt.expression.prefix_expression;

    if (.@"-" != exp.token.type) {
        return error.UnexpecedOperator;
    }

    var integer = exp.right.integer_literal;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }
}

test "parse Prefix OP (-)" {
    const allocator = std.testing.allocator;
    const input = "-5;";
    const output = -5;

    var lexer = Lexer.init(input);
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    var stmt = program.statements.items[0].expression_statement;

    var exp = stmt.expression.prefix_expression;

    if (.@"-" != exp.token.type) {
        return error.UnexpecedOperator;
    }

    var integer = exp.right.integer_literal;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }
}

test "Eval Integer Literal Expression" {
    const allocator = std.testing.allocator;
    const input = "5;";

    var lexer = Lexer.init(input);

    var parser = try Parser.new(allocator, &lexer);
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

    const literal = stmt.expression_statement.expression.integer_literal;

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

    var parser = try Parser.new(allocator, &lexer);
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

    const ident = stmt.expression_statement.expression.identifier;
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
    var parser = try Parser.new(allocator, &lexer);
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
    var p = try Parser.new(allocator, &lexer);
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
