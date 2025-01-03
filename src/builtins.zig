const std = @import("std");
const Object = @import("Object.zig");
const Value = Object.Value;
const memory = @import("memory.zig");
const Vm = @import("Vm.zig");

pub var list = [_]Object.Builtin{
    .{ .name = "@print", .function = printBuiltin },
    .{ .name = "len", .function = lenBuiltin },
    .{ .name = "@append", .function = appendBuiltin },
    .{ .name = "@read", .function = readBuiltin },
    .{ .name = "@write", .function = writeBuiltin },
};

pub fn readBuiltin(vm: *Vm, arg: []const Value) Value {
    if (arg[0] != .obj) return .null;

    switch (arg[0].obj.type) {
        .string => |str| {
            const file = str;
            const content = std.fs.cwd().readFileAlloc(vm.allocator, file, std.math.maxInt(usize)) catch return .null;
            const obj = vm.allocator.create(Object) catch return .null;
            obj.type = .{ .string = content };
            vm.instantiateAtVm(obj) catch return .null;
            return .{ .obj = obj };
        },

        else => return .null,
    }
}

pub fn writeBuiltin(_: *Vm, arg: []const Value) Value {
    if (arg.len != 2) return .null;

    if (arg[0] != .obj) return .null;
    if (arg[1] != .obj) return .null;

    const filename = arg[0].obj.type.string;
    const content = arg[1].obj.type.string;

    var file = std.fs.cwd().createFile(filename, .{}) catch return .null;
    defer file.close();
    file.writeAll(content) catch return .null;

    return .{ .integer = @intCast(content.len) };
}

pub fn lenBuiltin(_: *Vm, arg: []const Value) Value {
    if (arg[0] != .obj) return .null;

    return switch (arg[0].obj.type) {
        .array => |arr| .{ .integer = @intCast(arr.items.len) },
        .string => |str| .{ .integer = @intCast(str.len) },
        .hash => |hash| .{ .integer = @intCast(hash.pairs.count()) },
        else => .null,
    };
}

pub fn appendBuiltin(_: *Vm, arg: []const Value) Value {
    if (arg[0] != .obj) return .null;

    switch (arg[0].obj.type) {
        .array => |*arr| {
            for (arg[1..]) |val| {
                arr.append(val) catch return .null;
            }
        },
        else => {},
    }
    return .null;
}

pub fn printBuiltin(_: *Vm, args: []const Value) Value {
    for (args) |arg| {
        print(arg);
        std.debug.print(" ", .{});
    }
    std.debug.print("\n", .{});
    return .null;
}

test {
    const builtValue = list[0];
    _ = builtValue.function(&.{ .null, .{ .integer = 10 } });
}

fn print(value: Value) void {
    switch (value) {
        .obj => |o| switch (o.type) {
            .string => |s| std.debug.print("{s}", .{s}),

            .array => |arr| {
                std.debug.print("[", .{});
                for (arr.items, 0..) |s, i| {
                    print(s);
                    if (i != arr.items.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print("]", .{});
            },

            .hash => |hash| {
                var pairs = hash.pairs.iterator();
                std.debug.print("{{", .{});
                while (pairs.next()) |entry| {
                    const pair = entry.value_ptr.*;
                    const key = pair.key;
                    const val = pair.value;
                    print(key);
                    std.debug.print(": ", .{});
                    print(val);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .function => {
                std.debug.print("[function]", .{});
            },

            .closure => {
                std.debug.print("[closure]", .{});
            },

            .@"struct" => |stu| {
                std.debug.print("[{}_struct {{", .{stu.index});
                defer std.debug.print("}}]", .{});
                var iter = stu.fields.iterator();
                while (iter.next()) |entry| {
                    const key = entry.key_ptr;
                    std.debug.print("{s} = ", .{key.*});
                    print(entry.value_ptr.*);
                    std.debug.print(", ", .{});
                }
            },

            // inline else => |x| std.debug.print("{}", .{x}),
        },

        .char => |s| std.debug.print("{c}", .{s}),

        .builtin => |b| {
            std.debug.print("[builtin_function:{s}]", .{b.name});
        },

        .null => {
            std.debug.print("null", .{});
        },

        .boolean => |b| {
            std.debug.print("{}", .{b});
        },

        .tag => |b| {
            std.debug.print(".{s}", .{b});
        },

        .range => |r| std.debug.print("[range:{s}:{}:{}]", .{ @tagName(r.value.*), r.start, r.end }),

        inline else => |o| std.debug.print("{}", .{o}),
    }
}
