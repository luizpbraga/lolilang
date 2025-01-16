const std = @import("std");
const Object = @import("Object.zig");
const Value = Object.Value;
const memory = @import("memory.zig");
const Vm = @import("Vm.zig");

pub var builtin_functions = [_]Object.Builtin{
    .{ .name = "@print", .function = Builtin.print },
    .{ .name = "@length", .function = Builtin.length },
    .{ .name = "@append", .function = Builtin.append },
    .{ .name = "@read", .function = Builtin.read },
    .{ .name = "@write", .function = Builtin.write },
    .{ .name = "@typeOf", .function = Builtin.typeOf },
    .{ .name = "@asChar", .function = Builtin.asChar },
    .{ .name = "@panic", .function = Builtin.panic },
};

const Builtin = struct {
    pub fn panic(vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError("Argument Mismatch; expect 1, got {}", .{arg.len});
        if (arg[0] != .obj) return vm.newError("Invalid Argument type {s}", .{arg[0].name()});
        return vm.newError("panic: {s}", .{arg[0].obj.type.string});
    }

    pub fn typeOf(vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError("Argument Mismatch; expect 1, got {}", .{arg.len});

        return switch (arg[0]) {
            .obj => |ob| switch (ob.type) {
                .instance => |i| .{ .tag = i.type.name orelse "instance" },
                else => .{ .tag = @tagName(ob.type) },
            },
            else => .{ .tag = @tagName(arg[0]) },
        };
    }

    pub fn asChar(vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError("Argument Mismatch; expect 1, got {}", .{arg.len});

        const value = arg[0];

        if (value == .integer) l: {
            const int = value.integer;
            if (int < 0 or int >= 256) {
                break :l;
            }
            return .{ .char = @intCast(value.integer) };
        } else if (value == .char) {
            return value;
        } else if (value == .boolean) {
            return .{ .char = if (value.boolean) 0 else 1 };
        } else if (value == .obj) {
            if (value.obj.type == .string) {
                if (value.obj.type.string.len == 1) {
                    return .{ .char = value.obj.type.string[0] };
                }
            }
        }

        return vm.newError("Invalid {s} to Char coercion", .{value.name()});
    }

    pub fn read(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len == 0) {
            const content = try std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator, '\n', 10000);
            const obj = try vm.allocator.create(Object);
            obj.type = .{ .string = content };
            try vm.instantiateAtVm(obj);
            return .{ .obj = obj };
        }

        if (arg[0] != .obj) return .null;

        switch (arg[0].obj.type) {
            .string => |str| {
                const file = str;
                const content = try std.fs.cwd().readFileAlloc(vm.allocator, file, std.math.maxInt(usize));
                const obj = try vm.allocator.create(Object);
                obj.type = .{ .string = content };
                try vm.instantiateAtVm(obj);
                return .{ .obj = obj };
            },

            else => return .null,
        }
    }

    pub fn write(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len != 2) return vm.newError("Argument Mismatch, expect 2, got {}", .{arg.len});

        if (arg[0] != .obj) return .null;
        if (arg[1] != .obj) return .null;

        const filename = arg[0].obj.type.string;
        const content = arg[1].obj.type.string;

        var file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(content);

        return .{ .integer = @intCast(content.len) };
    }

    pub fn length(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len != 1) return vm.newError("Argument Mismatch", .{});

        if (arg[0] != .obj) return vm.newError("Type '{s}' don't have a length", .{arg[0].name()});

        return switch (arg[0].obj.type) {
            .array => |arr| .{ .integer = @intCast(arr.items.len) },
            .string => |str| .{ .integer = @intCast(str.len) },
            .hash => |hash| .{ .integer = @intCast(hash.pairs.count()) },
            else => vm.newError("Type '{s}' don't have a length", .{arg[0].name()}),
        };
    }

    pub fn append(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg[0] != .obj) return vm.newError("Invalid operation: Nothing to append to type '{s}'", .{arg[0].name()});

        switch (arg[0].obj.type) {
            .array => |*arr| {
                for (arg[1..]) |val| {
                    try arr.append(val);
                }
            },
            .string => |*str| {
                for (arg[1..]) |value| {
                    const string = switch (value) {
                        .obj => |ob| switch (ob.type) {
                            .string => |s| try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ str.*, s }),
                            else => return .null,
                        },
                        inline .integer, .float, .boolean => |val| try std.fmt.allocPrint(vm.allocator, "{s}{}", .{ str.*, val }),
                        .tag => |val| try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ str.*, val }),
                        .char => |val| try std.fmt.allocPrint(vm.allocator, "{s}{c}", .{ str.*, val }),
                        else => return vm.newError("Invalid string operation: Cannot concat type '{s}'", .{value.name()}),
                    };
                    // const string = std.mem.allocPrint(vm.allocator, "{s}{}", .{ str.*, val }) catch .null;
                    const obj = try memory.allocateObject(vm, .{ .string = string });
                    return .{ .obj = obj };
                }
            },
            else => {},
        }
        return .null;
    }

    pub fn print(_: *Vm, args: []const Value) anyerror!Value {
        for (args) |arg| {
            printV(arg);
            std.debug.print(" ", .{});
        }
        std.debug.print("\n", .{});
        return .null;
    }
};

test {
    const builtValue = builtin_functions[0];
    _ = try builtValue.function(&.{ .null, .{ .integer = 10 } });
}

fn printV(value: Value) void {
    switch (value) {
        .obj => |o| switch (o.type) {
            .string => |s| std.debug.print("{s}", .{s}),

            .array => |arr| {
                std.debug.print("[", .{});
                for (arr.items, 0..) |s, i| {
                    printV(s);
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
                    printV(key);
                    std.debug.print(": ", .{});
                    printV(val);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },

            .function => |f| {
                std.debug.print("fn {s}()", .{f.name orelse "?"});
            },

            .desc => |f| {
                var typename: []const u8 = "";

                if (f.method) |m| {
                    if (m.type == .instance) {
                        typename = m.type.instance.type.name orelse "";
                    }
                }

                std.debug.print("fn {s}.{s}()", .{ typename, f.name orelse "?" });
            },

            .closure => {
                std.debug.print("[closure]", .{});
            },

            .type => |ty| {
                std.debug.print("{s}_{s}", .{ @tagName(ty.type), ty.name orelse "annon" });
            },

            .instance => |ty| {
                std.debug.print("{s}{{", .{ty.type.name orelse "annon"});
                var iter = ty.fields.iterator();
                while (iter.next()) |entry| {
                    const key = entry.key_ptr;
                    const val = entry.value_ptr;

                    if (val.* == .obj) if (val.obj.type == .desc) continue;

                    std.debug.print("{s}: ", .{key.*});
                    printV(val.*);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}", .{});
            },
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
            std.debug.print("{s}", .{b});
        },

        .range => |r| std.debug.print("[range:{s}:{}:{}]", .{ @tagName(r.value.*), r.start, r.end }),

        inline else => |o| std.debug.print("{}", .{o}),
    }
}
