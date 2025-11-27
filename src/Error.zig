const std = @import("std");
const Allocator = std.mem.Allocator;

const Error = @This();

msg: std.ArrayList(u8) = .{},
input: []const u8 = "",
file: []const u8 = "",

pub const BOLD = "\x1b[1m";
pub const RED = "\x1b[31m";
pub const GREEN = "\x1b[32m";
pub const END = "\x1b[0m";

const Type = enum {
    syntax,
    compilation,
    runtime,

    fn msg(t: Type) []const u8 {
        return switch (t) {
            .syntax => "Syntax Error",
            .compilation => "Compilation Error",
            .runtime => "Runtime Error",
        };
    }
};

pub fn deinit(err: *Error, arena: Allocator) void {
    err.msg.deinit(arena);
}

pub fn append(err: *Error, arena: Allocator, comptime fmt: []const u8, args: anytype) !void {
    try err.msg.print(arena, fmt, args);
}

pub fn check(err: *Error) bool {
    return err.msg.items.len == 0;
}

pub fn writeError(err: *Error, stderr: std.fs.File) !void {
    try stderr.writeAll(err.msg.items);
}
