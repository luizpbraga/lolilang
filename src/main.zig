const std = @import("std");
const loli = @import("loli.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer {
        _ = gpa.detectLeaks();
        _ = gpa.deinit();
    }
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next();

    const file_name = if (args.next()) |file| file else {
        // FIX: return try loli.startRepl(allocator);
        return error.MissingFile;
    };

    const loli_args = args.next();

    if (!std.mem.endsWith(u8, file_name, ".loli")) {
        std.log.err("unrecognized file extension", .{});
        return;
    }

    if (loli_args) |arg| {
        if (std.mem.eql(u8, arg, "--emit-bytecode")) {
            loli.emitbytecode = true;
        }

        if (std.mem.eql(u8, arg, "--eb")) {
            loli.emitbytecode = true;
        }
    }

    const input: []const u8 = std.fs.cwd().readFileAlloc(allocator, file_name, std.math.maxInt(usize)) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.err("file {s} not found", .{file_name});
            return;
        },
        else => return err,
    };
    defer allocator.free(input);

    try loli.runVm(allocator, input);
}
