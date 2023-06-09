const std = @import("std");
// const Parser = @import("Parser.zig");

const line_str = "var x = {1,2,3,4,]";
const posiline: usize = line_str.len;

const RuntimeError = struct {
    line: []const u8 = line_str,
    posi: usize = posiline,
    line_number: usize = 0,

    fn unknowToken(self: *@This(), tok: u8) !void {
        std.debug.print("Error: Unknow Token '{c}' at line {d}:{d}: \n", .{ tok, self.line_number, self.posi });
        std.debug.print("   {s}\n", .{self.line});

        const allocator = std.heap.page_allocator;
        var x = try allocator.alloc(u8, self.posi);
        for (0..self.posi - 1) |i| x[i] = '-';
        x[self.posi - 1] = '^';
        std.debug.print("   {s}\n", .{x});
        allocator.free(x);
    }

    fn unknowIdentfier(self: *@This(), ident: []const u8) !void {
        std.debug.print("Error: Unknow Identifier '{s}' at line {d}:{d}: \n", .{ ident, self.line_number, self.posi });
        std.debug.print("   {s}\n", .{self.line});

        const allocator = std.heap.page_allocator;
        var x = try allocator.alloc(u8, self.posi);
        for (0..self.posi - 1) |i| x[i] = '-';
        x[self.posi - 1] = '^';
        std.debug.print("   {s}\n", .{x});
        allocator.free(x);
    }

    fn missingToken(self: *@This(), tok: u8) !void {
        std.debug.print("Error: Missing token '{c}' at line {d}:{d}: \n", .{ tok, self.line_number, self.posi });
        std.debug.print("   {s}\n", .{self.line});

        const allocator = std.heap.page_allocator;
        var x = try allocator.alloc(u8, self.posi + 1);
        for (0..self.posi) |i| x[i] = '-';
        x[self.posi] = '^';
        std.debug.print("   {s}\n", .{x});
        allocator.free(x);
    }
};

pub fn main() !void {
    var r = RuntimeError{};

    try r.unknowToken(']');
    r.line = "var x = {1,2,3,4,";
    r.posi = r.line.len;
    try r.missingToken('}');
    r.line = "var x = fn() { y = 10 }";
    r.posi = r.line.len - 7;
    try r.unknowIdentfier("y");

    // code
}
