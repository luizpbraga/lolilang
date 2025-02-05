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
    .{ .name = "@as", .function = Builtin.as },
    .{ .name = "@panic", .function = Builtin.panic },
    .{ .name = "@import", .function = Builtin.import },
    .{ .name = "@new", .function = Builtin.new },
    .{ .name = "@parse", .function = Builtin.parse },
    .{ .name = "@abs", .function = Builtin.abs },
    .{ .name = "@complex", .function = Builtin.complex },
    .{ .name = "@fetch", .function = Builtin.fetch },
    .{ .name = "@assert", .function = Builtin.assert },
    .{ .name = "@time", .function = Builtin.time },
    .{ .name = "@rand", .function = Builtin.rand },
};

const LoliType = enum {
    int,
    float,
    bool,
    char,
};

const tagtype = std.StaticStringMap(LoliType).initComptime(.{
    .{ "int", .int },
    .{ "float", .float },
    .{ "bool", .bool },
    .{ "char", .char },
});

fn defaultValue(ty: LoliType) Value {
    return switch (ty) {
        .int => .{ .integer = 0 },
        .float => .{ .float = 0 },
        .char => .{ .char = 0 },
        .bool => .{ .boolean = true },
        // .string => try newString(vm, null),
    };
}

pub fn newString(vm: *Vm, value: ?[]const u8) !Value {
    const bytes = value orelse "";

    var string = try std.ArrayList(u8).initCapacity(vm.allocator, bytes.len);
    errdefer string.deinit();
    try string.appendSlice(bytes);

    const obj = try memory.allocateObject(vm, .{ .string = string });
    return .{ .obj = obj };
}

/// TODO: THIS NEED A BIIIIG REWRITE
const Builtin = struct {
    pub fn time(_: *Vm, _: []const Value) !Value {
        return .{ .long_int = std.time.nanoTimestamp() };
    }

    pub fn rand(_: *Vm, args: []const Value) !Value {
        var prng = std.Random.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            try std.posix.getrandom(std.mem.asBytes(&seed));
            break :blk seed;
        });
        const r = prng.random();

        if (args.len == 2) {
            const i = args[0].integer;
            const f = args[1].integer;
            return .{ .integer = r.intRangeAtMost(i32, i, f) };
        }

        return .{ .integer = r.int(i32) };
    }

    pub fn assert(vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 2) return vm.newError("Argument Mismatch; expect 1 or 2 got {}", .{len});

        const boolean = arg[0].boolean;
        if (boolean) return .null;

        const string = if (len == 2) arg[1].obj.type.string.items else "Failed Assert";
        return vm.newError("{s}", .{string});
    }

    pub fn fetch(vm: *Vm, arg: []const Value) !Value {
        var cli: std.http.Client = .{ .allocator = vm.allocator };
        defer cli.deinit();

        var string: std.ArrayList(u8) = .init(vm.allocator);
        errdefer string.deinit();

        const res = try cli.fetch(.{
            .location = .{ .url = arg[0].obj.type.string.items },
            .response_storage = .{ .dynamic = &string },
            .max_append_size = std.math.maxInt(usize),
        });

        if (res.status != .ok) {
            string.deinit();
            return .null;
        }

        const obj = try memory.allocateObject(vm, .{ .string = string });
        return .{ .obj = obj };
    }

    pub fn complex(vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 2) return vm.newError("Argument Mismatch; expect 1 or 2 got {}", .{len});

        const real: f32 = switch (arg[0]) {
            .integer => |i| @floatFromInt(i),
            .float => |f| f,
            else => 0,
        };

        const imag: f32 = switch (arg[1]) {
            .integer => |i| @floatFromInt(i),
            .float => |f| f,
            else => 0,
        };

        return .{ .complex = .{ .real = real, .imag = imag } };
    }

    pub fn abs(vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len != 1) return vm.newError("Argument Mismatch; expect 1 got {}", .{len});

        return switch (arg[0]) {
            .integer => |i| .{ .integer = @intCast(@abs(i)) },
            .float => |i| .{ .float = @abs(i) },
            .complex => |z| .{ .float = @sqrt(z.real * z.real + z.imag * z.imag) },
            else => |t| vm.newError("Type Mismatch; expect integer or float, got {s}", .{t.name()}),
        };
    }

    pub fn parse(vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len != 2) return vm.newError("Argument Mismatch; expect 2", .{});

        if (arg[0] != .tag) {
            return vm.newError("Expect type/tag, found {s}", .{arg[0].name()});
        }

        const tag = arg[0].tag;
        const ty = tagtype.get(tag) orelse return vm.newError("Invalid Type '{s}'", .{tag});

        const string = arg[1].obj.type.string.items;

        return switch (ty) {
            .int => .{ .integer = try std.fmt.parseInt(i32, string, 10) },
            .float => .{ .float = try std.fmt.parseFloat(f32, string) },
            else => vm.newError("Invalid type {}", .{ty}),
        };
    }

    pub fn new(vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 3) return vm.newError("Argument Mismatch; expect at least 3", .{});

        if (arg[0] != .tag) {
            return vm.newError("Expect type/tag, found {s}", .{arg[0].name()});
        }

        const tag = arg[0].tag;
        const ty = tagtype.get(tag) orelse return vm.newError("Invalid Type '{s}'", .{tag});
        var default = defaultValue(ty);
        const capacity: usize = if (len == 2 or len == 3) @intCast(arg[1].integer) else return default;

        var array = try std.ArrayList(Value).initCapacity(vm.allocator, capacity);
        errdefer array.deinit();

        if (arg.len == 3) default = arg[2];
        for (0..capacity) |_| try array.append(default);

        const obj = try memory.allocateObject(vm, .{ .array = array });
        return .{ .obj = obj };
    }

    pub fn import(vm: *Vm, arg: []const Value) !Value {
        const filename = arg[0].obj.type.string.items;
        var l: @import("Lexer.zig") = .init(filename);
        var p: @import("Parser.zig") = .init(vm.allocator, &l, vm.errors);
        defer p.deinit();
        const node = try p.parse();
        _ = node;
        // errdefer std.debug.print("-->> {s}\n\n", .{vm.compiler.errors.msg.items});
        // errdefer std.debug.print("-->> {s}\n\n", .{vm.errors.msg.items});
        // try vm.compiler.compile(node);
        // const code = try vm.compiler.bytecode();
        // // defer code.deinit;
        // vm.constants = try std.mem.concat(vm.allocator, Value, &.{ vm.constants, code.constants });
        // vm.positions = try std.mem.concat(vm.allocator, usize, &.{ vm.positions, code.positions });
        return .null;
    }

    pub fn panic(vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError("Argument Mismatch; expect 1, got {}", .{arg.len});
        if (arg[0] != .obj) return vm.newError("Invalid Argument type {s}", .{arg[0].name()});
        return vm.newError("panic: {s}", .{arg[0].obj.type.string.items});
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

    pub fn as(vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 2) return vm.newError("Argument Mismatch; expect 2, got {}", .{arg.len});

        const ty = tagtype.get(arg[0].tag) orelse return vm.newError("Invalid Type", .{});
        const value = arg[1];

        switch (ty) {
            .char => {
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
                        if (value.obj.type.string.items.len == 1) {
                            return .{ .char = value.obj.type.string.items[0] };
                        }
                    }
                }
            },

            else => {},
        }

        return vm.newError("Invalid {s} to Char coercion", .{value.name()});
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
                if (value.obj.type.string.items.len == 1) {
                    return .{ .char = value.obj.type.string.items[0] };
                }
            }
        }

        return vm.newError("Invalid {s} to Char coercion", .{value.name()});
    }

    pub fn read(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len == 0) {
            const bytes = try std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator, '\n', 10000);
            defer vm.allocator.free(bytes);
            return try newString(vm, bytes);
        }

        if (arg[0] != .obj) return .null;

        switch (arg[0].obj.type) {
            .string => |str| {
                const file = str.items;
                const bytes = std.fs.cwd().readFileAlloc(vm.allocator, file, std.math.maxInt(usize)) catch |err| switch (err) {
                    error.FileNotFound => return .null,
                    else => return err,
                };

                defer vm.allocator.free(bytes);
                return try newString(vm, bytes);
            },

            else => return .null,
        }
    }

    pub fn write(vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len != 2) return vm.newError("Argument Mismatch, expect 2, got {}", .{arg.len});

        if (arg[0] != .obj) return .null;
        if (arg[1] != .obj) return .null;

        const filename = arg[0].obj.type.string.items;
        const content = arg[1].obj.type.string.items;

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
            .string => |str| .{ .integer = @intCast(str.items.len) },
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
                for (arg[1..]) |val| {
                    if (val == .obj and val.obj.type == .string) {
                        try str.appendSlice(val.obj.type.string.items);
                        continue;
                    }

                    switch (val) {
                        inline .integer, .float => |x| try str.writer().print("{d}", .{x}),
                        .char => |c| try str.append(c),
                        .boolean => |b| try str.appendSlice(if (b) "true" else "false"),
                        else => {},
                    }
                }
            },
            // .string => |*str| {
            //     for (arg[1..]) |value| {
            //         const string = switch (value) {
            //             .obj => |ob| switch (ob.type) {
            //                 .string => |s| try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ str.*, s }),
            //                 else => return .null,
            //             },
            //             inline .integer, .float, .boolean => |val| try std.fmt.allocPrint(vm.allocator, "{s}{}", .{ str.*, val }),
            //             .tag => |val| try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ str.*, val }),
            //             .char => |val| try std.fmt.allocPrint(vm.allocator, "{s}{c}", .{ str.*, val }),
            //             else => return vm.newError("Invalid string operation: Cannot concat type '{s}'", .{value.name()}),
            //         };
            //         // const string = std.mem.allocPrint(vm.allocator, "{s}{}", .{ str.*, val }) catch .null;
            //         const obj = try memory.allocateObject(vm, .{ .string = string });
            //         return .{ .obj = obj };
            //     }
            // },
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

// test {
//     const builtValue = builtin_functions[0];
//     _ = try builtValue.function(&.{ .null, .{ .integer = 10 } });
// }

/// TODO: use a writer
fn printV(value: Value) void {
    switch (value) {
        .obj => |o| switch (o.type) {
            .string => |s| std.debug.print("{s}", .{s.items}),

            .array => |arr| {
                std.debug.print("[", .{});
                for (arr.items) |s| {
                    printV(s);
                    std.debug.print(", ", .{});
                }

                if (arr.items.len != 0) {
                    std.debug.print("\x1b[2D]", .{});
                    return;
                }

                std.debug.print("]", .{});
            },

            // else => {},
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

                if (hash.pairs.count() != 0) {
                    std.debug.print("\x1b[2D}}", .{});
                    return;
                }

                std.debug.print("}}", .{});
            },

            .function => |f| {
                std.debug.print("fn {s}()", .{f.name orelse "?"});
            },

            .decl => |f| {
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
                std.debug.print("{s}", .{ty.name.?});
            },

            .namespace => |n| {
                std.debug.print("{s}", .{n.name});
            },

            .instance => |ty| {
                std.debug.print("{s}{{", .{ty.type.name orelse "annon"});
                var n: usize = 0;
                var iter = ty.fields.iterator();
                while (iter.next()) |entry| {
                    const key = entry.key_ptr;
                    const val = entry.value_ptr;

                    if (val.* == .obj) if (val.obj.type == .decl) continue;

                    std.debug.print("{s}: ", .{key.*});
                    printV(val.*);
                    std.debug.print(", ", .{});
                    n += 1;
                }

                if (n != 0) {
                    std.debug.print("\x1b[2D}}", .{});
                    return;
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

        .complex => |z| {
            std.debug.print("({d}, {d})", .{ z.real, z.imag });
        },

        .boolean => |b| {
            std.debug.print("{}", .{b});
        },

        .tag => |b| {
            std.debug.print("{s}", .{b});
        },

        .enumtag => |et| {
            std.debug.print("{s}", .{et.tag});
        },

        .range => |r| std.debug.print("[range:{s}:{}:{}]", .{ @tagName(r.value.*), r.start, r.end }),

        .float => |o| std.debug.print("{d}", .{o}),

        inline else => |o| std.debug.print("{}", .{o}),
    }
}
