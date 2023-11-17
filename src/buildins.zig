const std = @import("std");
const object = @import("object.zig");
const Environment = @import("./Environment.zig");

pub const buildins = std.ComptimeStringMap(object.Builtin, .{
    .{ "print", .{ .func = printBuiltin } },
    .{ "readFile", .{ .func = readFileBuiltin } },
    .{ "closeFile", .{ .func = closeFileBuiltin } },
});

const calloc = std.heap.c_allocator;

fn readFile(file_path: []const u8) ?[]const u8 {
    return std.fs.cwd().readFileAlloc(calloc, file_path, std.math.maxInt(usize)) catch null;
}

fn readFileBuiltin(args: []const object.Object) object.Object {
    const file_path = args[0].string.value;
    const content = readFile(file_path) orelse return object.NULL;
    return object.Object{ .string = .{ .value = content } };
}

fn closeFileBuiltin(args: []const object.Object) object.Object {
    const string_to_kill = args[0].string.value;
    calloc.free(string_to_kill);
    return object.NULL;
}

fn pprint(arg: *const object.Object) void {
    switch (arg.*) {
        .null => |n| std.debug.print("{any}", .{n.value}),
        .float => |n| std.debug.print("{d}", .{n.value}),
        .string => |n| std.debug.print("{s}", .{n.value}),
        .integer => |n| std.debug.print("{}", .{n.value}),
        .boolean => |n| std.debug.print("{}", .{n.value}),
        else => std.debug.print("Not yet printable\n", .{}),
    }
}

fn printBuiltin(args: []const object.Object) object.Object {
    for (args) |arg| {
        switch (arg) {
            .enum_tag => |n| std.debug.print(".{s} ", .{n.name}),
            .null => |n| std.debug.print("{any} ", .{n.value}),
            .float => |n| std.debug.print("{d} ", .{n.value}),
            .string => |n| std.debug.print("{s} ", .{n.value}),
            .integer => |n| std.debug.print("{} ", .{n.value}),
            .boolean => |n| std.debug.print("{} ", .{n.value}),
            .array => |n| {
                std.debug.print("[ ", .{});

                for (n.elements, 0..) |*el, i| {
                    pprint(el);
                    if (i < n.elements.len - 1) std.debug.print(", ", .{});
                }

                std.debug.print(" ]", .{});
            },
            // .hash => |h| {
            //     var iter = h.pairs.iterator();
            //     std.debug.print("{{ ", .{});
            //     while (iter.next()) |entry| {
            //         pprint(entry.key_ptr);
            //         std.debug.print(":", .{});
            //         pprint(entry.value_ptr.*);
            //         std.debug.print(", ", .{});
            //     }
            //     std.debug.print("}}{c}", .{ch});
            // },
            else => std.debug.print("Not printable yet", .{}),
        }
    }
    std.debug.print("\n", .{});

    return object.NULL;
}
