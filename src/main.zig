const std = @import("std");
const loli = @import("loli.zig");
const fmt = @import("fmt.zig");
const builtin = @import("builtin");

pub fn main() !void {
    var debug_alloc: std.heap.DebugAllocator(.{}) = .init;

    const allocator, const is_debug = switch (builtin.mode) {
        else => .{ debug_alloc.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };

    defer if (is_debug) {
        _ = debug_alloc.deinit();
    };

    var args = std.process.args();
    _ = args.next();

    const file_name = args.next() orelse return std.log.err("Missing File", .{});
    if (!std.mem.endsWith(u8, file_name, ".loli")) {
        std.log.err("unrecognized file extension", .{});
        return;
    }

    const input: []const u8 = std.fs.cwd().readFileAlloc(file_name, allocator, .unlimited) catch |err| switch (err) {
        error.FileNotFound => {
            return std.log.err("File {s} not found", .{file_name});
        },
        else => return err,
    };
    defer allocator.free(input);

    const loli_args = args.next();
    if (loli_args) |arg| f: {
        // if (std.mem.eql(u8, arg, "--fmt")) {
        //     try fmt.format(allocator, file_name, input, &err);
        //     return;
        // }
        if (std.mem.eql(u8, arg, "--emit-bytecode")) {
            loli.emitbytecode = true;
            break :f;
        }
        if (std.mem.eql(u8, arg, "--eb")) {
            loli.emitbytecode = true;
            break :f;
        }
    }

    try loli.runVm(allocator, input);
}
