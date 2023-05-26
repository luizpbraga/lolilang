const std = @import("std");
const stdin = std.io.getStdIn();
const stdout = std.io.getStdOut();
const Lexer = @import("Lexer.zig");

fn start() !void {
    var buffer: [200]u8 = undefined;
    var size: usize = undefined;

    std.debug.print("Repl 0.0.1 (max 200 characters per command)\n", .{});

    while (true) {
        std.debug.print(">> ", .{});

        size = try stdin.read(&buffer);
        const response = buffer[0..size];
        if (std.mem.eql(u8, "exit\n", response)) break;
        std.debug.print("{s}", .{response});

        var lexer = Lexer.init(response);
        var tok = lexer.nextToken();

        while (tok.type != .eof) {
            tok = lexer.nextToken();
            std.debug.print("{} {s}\n", .{ tok.type, tok.literal });
        }
    }
}

pub fn main() !void {
    try start();
    std.debug.print("\n{s}\n", .{"loli go brrr"});
}
