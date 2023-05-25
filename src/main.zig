const std = @import("std");
const Lexer = @import("Lexer.zig");

pub fn main() !void {
    const input =
        \\var five = 5;
        \\var ten = 10;
        \\const add = fn(x, y) {
        \\  return (x + y) / 2 * 100 > 1;
        \\};
        \\var result = if (x > 2) add(five, ten) < else !true;
        \\const p = 5 == 3;
    ;

    var l = Lexer.init(input);
    while (true) {
        var t = l.nextToken();
        std.debug.print("{} {s}\n", .{ t.type, t.literal });
        if (t.type == .eof) break;
    }
}

test "simple test" {}
