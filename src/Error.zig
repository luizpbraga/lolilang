const std = @import("std");

const Error = @This();

msg: std.ArrayList(u8),
input: []const u8,

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

pub fn deinit(err: *Error) void {
    err.msg.deinit();
}

fn append(err: *Error, comptime fmt: []const u8, args: anytype) !void {
    try err.msg.writer().print(fmt, args);
    err.counter += 1;
}
