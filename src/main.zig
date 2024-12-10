const std = @import("std");
const loli = @import("loli.zig");

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    var args = std.process.args();
    _ = args.next();

    const file_name = if (args.next()) |file| file else {
        var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
        const alloc = gpa.allocator();
        defer {
            _ = gpa.detectLeaks();
            _ = gpa.deinit();
        }
        return try loli.startRepl(alloc);
    };

    if (!std.mem.endsWith(u8, file_name, ".loli")) {
        std.log.err("unrecognized file extension", .{});
        return;
    }

    const input: []const u8 = std.fs.cwd().readFileAlloc(allocator, file_name, 1024) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.err("file {s} not found", .{file_name});
            return;
        },
        else => return err,
    };
    defer allocator.free(input);

    try loli.runVm(allocator, input);
}
