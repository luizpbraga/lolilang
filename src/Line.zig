const std = @import("std");

const Line = @This();

line: []const u8,
start: usize,
end: usize,
index: usize,

pub fn init(input: []const u8, position: usize) Line {
    const pos = if (position > input.len) input.len else position;

    const index = std.mem.count(u8, input[0..pos], "\n") + 1;
    const start = std.mem.lastIndexOf(u8, input[0..pos], "\n") orelse 0;
    var end = std.mem.indexOf(u8, input[pos..], "\n") orelse input.len;
    if (end != input.len) end += pos;
    return .{ .line = std.mem.trim(u8, input[start..end], "\n"), .end = end, .start = pos - start, .index = index };
}

test {
    const input =
        \\  ola eu sou
        \\  o netinho de paula
        \\fn foo(): 10
    ;
    const line = init(input, 100);
    std.debug.print("{s}|{}\n", .{ line.line, line.index });
}
