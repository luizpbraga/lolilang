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
    const x = _printBuiltin(args, ' ');
    std.debug.print("\n", .{});
    return x;
}

fn _printBuiltin(args: []const object.Object, ch: u8) object.Object {
    for (args) |arg| {
        switch (arg) {
            .enum_tag => |n| std.debug.print(".{s}{c}", .{ n.name, ch }),
            .null => |n| std.debug.print("{any}{c}", .{ n.value, ch }),
            .float => |n| std.debug.print("{d}{c}", .{ n.value, ch }),
            .string => |n| std.debug.print("{s}{c}", .{ n.value, ch }),
            .integer => |n| std.debug.print("{}{c}", .{ n.value, ch }),
            .boolean => |n| std.debug.print("{}{c}", .{ n.value, ch }),
            .array => |n| {
                std.debug.print("[ ", .{});

                for (n.elements, 0..) |*el, i| {
                    pprint(el);
                    if (i < n.elements.len - 1) std.debug.print(", ", .{});
                }

                std.debug.print(" ]{c}", .{ch});
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
            else => std.debug.print("Not yet printable\n", .{}),
        }
    }
    if (args.len > 1) std.debug.print("\n", .{});

    return object.NULL;
}
