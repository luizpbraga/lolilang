const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;

pub fn main() !void {
    // const allocator = std.heap.page_allocator;
    // var lexer = Lexer.init("-69 - 1");
    // var parser = try Parser.new(allocator, &lexer);
    // defer parser.deinit();

    // const program = try parser.parseProgram(allocator);
    // defer program.statements.deinit();
    const file =
        \\var y = 11;
        \\var y, = 10;
        \\var y = 11;
    ;

    const syntax_error_char = 5;
    const syntax_error_line = 0;
    _ = syntax_error_line;

    const number_of_lines = std.mem.count(u8, file, "\n");
    _ = number_of_lines;

    const last = std.mem.indexOf(u8, file[5..], "\n").?;
    std.debug.print("{} \n", .{last});
    std.debug.print(">{s}", .{file[syntax_error_char .. last + syntax_error_char]});
}
