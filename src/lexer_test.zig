const std = @import("std");
const Lexer = @import("Lexer.zig");

test {
    const input =
        \\var x = 1;
        \\var y = "ok"
        \\
        \\      a
        \\vacilao morre
    ;

    var lex: Lexer = .init(input);
    //defer lex.deinit();

    std.debug.print("\n", .{});
    while (true) {
        const tk = lex.nextToken();
        if (tk.type == .eof or tk.type == .illegal) break;
        // std.debug.print("({},{},{}) {s} {s}\n", .{ tk.x, tk.y, tk.x, @tagName(tk.type), tk.literal });
    }
}
