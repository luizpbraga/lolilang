const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("asc.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input =
        \\ var add = fn(x,y) { return x+y;};
    ;

    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();
    var p = Parser.new(&lexer);

    var program = try p.parseProgram(allocator);
    defer program.statements.deinit();

    std.debug.print("{}", .{program.statements.items.len});

    while (tok.type != .eof) {
        std.debug.print("{} {s}\n", .{ tok.type, tok.literal });
        tok = lexer.nextToken();
    }
}

test "simple test" {}
