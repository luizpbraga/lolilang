const std = @import("std");
const object = @import("object.zig");

fn printBuiltin(args: []const object.Object) object.Object {
    for (args) |arg| {
        switch (arg) {
            .null => |n| std.debug.print("{any}\n", .{n.value}),
            .integer => |n| std.debug.print("{any}\n", .{n.value}),
            .string => |n| std.debug.print("{s}\n", .{n.value}),
            .boolean => |n| std.debug.print("{s}\n", .{n.value}),
            .array => |n| std.debug.print("{any}\n", .{n.elements}),
            else => std.debug.print("not yet printable\n", .{}),
        }
    }

    return .{ .null = object.Null{} };
}

buildins: std.ComptimeStringMap(object.Builtin, .{
    .{ "print", .{ .func = printBuiltin } },
}),
