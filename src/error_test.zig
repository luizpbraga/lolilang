const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const TokenType = @import("Token.zig").TokenType;

const talloc = std.testing.allocator;

test "Parse VAR statements" {
    const expected_value = struct {
        i64, // bool, []const u8,
    }{
        5, // true, "y"
    };

    const tests = [_]struct {
        input: []const u8,
        expected_indetifier: []const u8,
    }{
        .{ .input = "var x = ", .expected_indetifier = "x" },
        // .{ .input = "var x = true;", .expected_indetifier = "x" },
        // .{ .input = "var x = y;", .expected_indetifier = "x" },
    };

    inline for (tests, expected_value) |x, k| {
        _ = k;
        var lexer = Lexer.init(x.input);

        var p = try Parser.new(talloc, &lexer);
        defer p.deinit();

        const program = try p.parseProgram();
        _ = program;
    }
}

pub fn main() !void {
    // code
}
