const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("asc.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input =
        \\return x;
        \\return 10;
        \\return 9999;
    ;

    var lexer = Lexer.init(input);
    var p = Parser.new(&lexer);

    var program = try p.parseProgram(allocator);
    defer program.statements.deinit();

    try std.testing.expect(program.statements.items.len == 3);

    // var tok = lexer.nextToken();
    // _ = tok;
    // while (tok.type != .eof) {
    //     std.debug.print("{} {s}\n", .{ tok.type, tok.literal });
    //     tok = lexer.nextToken();
    // }
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
