const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("asc.zig");

pub fn main() !void {}

test "eval Program" {
    const allocator = std.heap.page_allocator;
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
                    .value = "anotherVar ",
                },
            },
        },
    };

    try stmts.append(stmt);

    var program = ast.Program{ .statements = stmts };
    std.debug.print("{s}", .{program.string()});
}

test "Parse RETURN statements: Size" {
    const allocator = std.testing.allocator;
    const input =
        \\return x;
        \\return 10;
        \\return 9999;
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.new(&lexer);
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
    var p = Parser.new(&lexer);
    var program = try p.parseProgram(allocator);
    defer program.statements.deinit();
    try std.testing.expect(program.statements.items.len == 3);
}

test "Token test" {
    const input =
        \\var x = 100 + if (!true) 5 else -10;
    ;
    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();

    while (tok.type != .eof) {
        std.debug.print("{} {s}\n", .{ tok.type, tok.literal });
        tok = lexer.nextToken();
    }
}
