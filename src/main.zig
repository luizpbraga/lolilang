const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("asc.zig");
pub fn main() !void {}
test "parse Prefix OP" {
    const allocator = std.testing.allocator;
    const input = "-5;";
    const output = -5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    // for (program.statements.items) |item|
    //     std.debug.print("value {}", .{item.expression_statement.expression.?.prefix_expression.right.?.integer_literal.value});

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }
    var stmt = program.statements.items[0];

    if (stmt != .expression_statement) {
        return error.ExprecAnExpression;
    }

    var exp = stmt.expression_statement.expression.?;

    if (exp != .prefix_expression) {
        return error.ExprectAnPrefixExpre;
    }

    if (.@"-" != exp.prefix_expression.token.type) {
        return error.UnexpecedOperator;
    }

    var right = exp.prefix_expression.right.?;
    var integer = right.integer_literal;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }

    // std.debug.print("{}", .{integer.value});
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
        return error.ExprecAnExpression;
    }

    const literal = stmt.expression_statement.expression.?.integer_literal;

    if (5 != literal.value) {
        return error.UnexpectValue;
    }

    if (!std.mem.eql(u8, "5", literal.tokenLiteral())) {
        return error.UnexpectValue;
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
        return error.ExprecAnExpression;
    }

    const ident = stmt.expression_statement.expression.?.identifier;
    const value = ident.value;

    if (!std.mem.eql(u8, "foobar", value)) {
        return error.UnexpectValue;
    }

    if (!std.mem.eql(u8, "foobar", ident.tokenLiteral())) {
        return error.UnexpectValue;
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
    std.debug.print("{s}", .{program.tokenLiteral()});
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
    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();

    while (tok.type != .eof) {
        std.debug.print("{} {s}\n", .{ tok.type, tok.literal });
        tok = lexer.nextToken();
    }
}
