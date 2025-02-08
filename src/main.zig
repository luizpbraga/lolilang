const std = @import("std");
const loli = @import("loli.zig");
const fmt = @import("fmt.zig");
const Error = @import("Error.zig");
const builtin = @import("builtin");

pub fn main() !void {
    var debug_alloc: std.heap.GeneralPurposeAllocator(.{}) = .{};

    const allocator, const is_debug = switch (builtin.mode) {
        else => .{ debug_alloc.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.c_allocator, false },
    };

    defer if (is_debug) {
        _ = debug_alloc.detectLeaks();
        _ = debug_alloc.deinit();
    };

    var args = std.process.args();
    _ = args.next();

    const file_name = if (args.next()) |file| file else {
        // FIX: return try loli.startRepl(allocator);
        std.log.err("Missing File", .{});
        return;
    };

    const loli_args = args.next();

    if (!std.mem.endsWith(u8, file_name, ".loli")) {
        std.log.err("unrecognized file extension", .{});
        return;
    }

    const input: []const u8 = std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(usize)) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.err("File {s} not found", .{file_name});
            return;
        },
        else => return err,
    };
    defer allocator.free(input);

    var err: Error = .{ .file = file_name, .input = input, .msg = .init(allocator) };
    defer err.deinit();

    if (loli_args) |arg| f: {
        if (std.mem.eql(u8, arg, "--fmt")) {
            try fmt.format(allocator, file_name, input, &err);
            return;
        }
        if (std.mem.eql(u8, arg, "--emit-bytecode")) {
            loli.emitbytecode = true;
            break :f;
        }
        if (std.mem.eql(u8, arg, "--eb")) {
            loli.emitbytecode = true;
            break :f;
        }
    }

    try loli.runVm(allocator, input, &err);
}
