const std = @import("std");
const Vm = @import("Vm.zig");
const gc = @import("gc.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Allocator = std.mem.Allocator;

pub const builtin_functions = [_]Object.Builtin{
    .{ .name = "@abs", .function = Builtin.abs },
    .{ .name = "@append", .function = Builtin.append },
    .{ .name = "@args", .function = Builtin.cmdLineArgs },
    .{ .name = "@as", .function = Builtin.as },
    .{ .name = "@asChar", .function = Builtin.asChar },
    .{ .name = "@assert", .function = Builtin.assert },
    .{ .name = "@complex", .function = Builtin.complex },
    .{ .name = "@cos", .function = Builtin.cos },
    .{ .name = "@fetch", .function = Builtin.fetch },
    .{ .name = "@frame", .function = Builtin.frameF },
    .{ .name = "@import", .function = Builtin.import },
    .{ .name = "@length", .function = Builtin.length },
    .{ .name = "@new", .function = Builtin.new },
    .{ .name = "@panic", .function = Builtin.panic },
    .{ .name = "@parse", .function = Builtin.parse },
    .{ .name = "@pop", .function = Builtin.pop },
    .{ .name = "@print", .function = Builtin.print },
    .{ .name = "@rand", .function = Builtin.rand },
    .{ .name = "@read", .function = Builtin.read },
    .{ .name = "@resume", .function = Builtin.resumeF },
    .{ .name = "@sin", .function = Builtin.sin },
    .{ .name = "@suspend", .function = Builtin.suspendF },
    .{ .name = "@syscall", .function = Builtin.syscall },
    .{ .name = "@tagName", .function = Builtin.tagName },
    .{ .name = "@tan", .function = Builtin.tan },
    .{ .name = "@time", .function = Builtin.time },
    .{ .name = "@typeOf", .function = Builtin.typeOf },
    .{ .name = "@write", .function = Builtin.write },
};

const LoliType = enum {
    bool,
    char,
    float,
    int,
};

const tagtype = std.StaticStringMap(LoliType).initComptime(.{
    .{ "bool", .bool },
    .{ "char", .char },
    .{ "float", .float },
    .{ "int", .int },
});

fn defaultValue(ty: LoliType) Value {
    return switch (ty) {
        .int => .{ .integer = 0 },
        .float => .{ .float = 0 },
        .char => .{ .char = 0 },
        .bool => .{ .boolean = true },
        // .string => try newString(gpa,vm, null),
    };
}

pub fn newString(gpa: Allocator, vm: *Vm, value: ?[]const u8) !Value {
    const bytes = value orelse "";
    var string: std.ArrayList(u8) = try .initCapacity(gpa, bytes.len);
    errdefer string.deinit(gpa);
    try string.appendSlice(gpa, bytes);
    const obj = try gc.allocObject(gpa, vm, .{ .string = string });
    return .{ .obj = obj };
}

/// TODO: THIS NEED A BIIIIG REWRITE
const Builtin = struct {
    pub fn printf(gpa: Allocator, vm: *Vm, values: []const Value) !Value {
        _ = gpa; // autofix
        _ = vm;
        _ = values;
        return .null;
    }

    pub fn cmdLineArgs(gpa: Allocator, vm: *Vm, _: []const Value) !Value {
        var iter = std.process.args();
        // skip path and file name
        _ = iter.next();
        _ = iter.next();

        var array: std.ArrayList(Value) = .empty;
        errdefer array.deinit(gpa);

        while (iter.next()) |item| {
            const string = try newString(gpa, vm, item);
            try array.append(gpa, string);
        }

        // we can not iterate over nulls, right?
        // this can cause runtime problens!!!!
        if (array.items.len == 0) {
            return .null;
        }

        const obj = try gc.allocObject(gpa, vm, .{ .array = array });
        return .{ .obj = obj };
    }

    pub fn cos(gpa: Allocator, vm: *Vm, args: []const Value) !Value {
        const arg: f32 = switch (args[0]) {
            .float => |f| f,
            .integer => |i| @floatFromInt(i),
            .char => |c| @floatFromInt(c),
            else => return vm.newError(gpa, "Invalid Argument; ", .{}),
        };
        return .{ .float = @cos(arg) };
    }

    pub fn sin(gpa: Allocator, vm: *Vm, args: []const Value) !Value {
        const arg: f32 = switch (args[0]) {
            .float => |f| f,
            .integer => |i| @floatFromInt(i),
            .char => |c| @floatFromInt(c),
            else => return vm.newError(gpa, "Invalid Argument; ", .{}),
        };
        return .{ .float = @sin(arg) };
    }

    pub fn tan(gpa: Allocator, vm: *Vm, args: []const Value) !Value {
        const arg: f32 = switch (args[0]) {
            .float => |f| f,
            .integer => |i| @floatFromInt(i),
            .char => |c| @floatFromInt(c),
            else => return vm.newError(gpa, "Invalid Argument; ", .{}),
        };
        return .{ .float = @tan(arg) };
    }

    /// FIX: segfault
    pub fn syscall(gpa: Allocator, vm: *Vm, args: []const Value) !Value {
        const argv = try gpa.alloc([]const u8, args.len);
        defer gpa.free(argv);

        for (args, 0..) |value, i| {
            argv[i] = value.obj.type.string.items;
        }

        const result = try std.process.Child.run(
            .{ .allocator = gpa, .argv = argv },
        );
        std.debug.print("{s}\n\n\n{s}", .{ result.stdout, result.stderr });
        defer gpa.free(result.stdout);
        defer gpa.free(result.stderr);

        const stdout = if (result.stdout.len == 0) .null else try newString(gpa, vm, result.stdout);
        const stderr = if (result.stderr.len == 0) .null else try newString(gpa, vm, result.stderr);

        var array = try std.ArrayList(Value).initCapacity(gpa, 2);
        errdefer array.deinit(gpa);

        try array.append(gpa, stdout);
        try array.append(gpa, stderr);

        const obj = try gc.allocObject(gpa, vm, .{ .array = array });
        return .{ .obj = obj };
    }

    pub fn resumeF(gpa: Allocator, _: *Vm, args: []const Value) !Value {
        _ = gpa; // autofix
        return .{ .frame = args[0].frame };
    }

    pub fn frameF(gpa: Allocator, vm: *Vm, _: []const Value) !Value {
        _ = gpa; // autofix
        return .{ .frame = vm.currentFrame() };
    }

    pub fn suspendF(gpa: Allocator, vm: *Vm, _: []const Value) !Value {
        _ = gpa; // autofix
        const frame = vm.popFrame(); // return to the callee frame
        vm.fm = vm.currentFrame(); // ?
        vm.sp = @intCast(frame.bp); // pop the fn
        vm.instructions = vm.fm.instructions();
        return .{ .frame = frame };
    }

    pub fn tagName(gpa: Allocator, vm: *Vm, args: []const Value) !Value {
        const name = switch (args[0]) {
            .enumtag => |t| t.tag,
            .tag => |t| t,
            else => return vm.newError(gpa, "Invalid Argument", .{}),
        };
        return newString(gpa, vm, name);
    }

    pub fn time(gpa: Allocator, _: *Vm, _: []const Value) !Value {
        _ = gpa; // autofix
        // TODO
        // return .{ .long_int = @intCast(@as(u64, (std.time.Instant.now() catch return .null).timestamp)) };
        return .null;
    }

    pub fn rand(gpa: Allocator, _: *Vm, args: []const Value) !Value {
        _ = gpa; // autofix
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

    pub fn assert(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 2) return vm.newError(gpa, "Argument Mismatch; expect 1 or 2 got {}", .{len});
        const boolean = arg[0].boolean;
        if (boolean) return .null;
        const string = if (len == 2) arg[1].obj.type.string.items else "Failed Assert";
        return vm.newError(gpa, "{s}", .{string});
    }

    pub fn fetch(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        _ = gpa; // autofix
        _ = vm; // autofix
        _ = arg; // autofix
        // var cli: std.http.Client = .{ .allocator = gpa };
        // defer cli.deinit();
        //
        // var string: std.ArrayList(u8) = .empty;
        // errdefer string.deinit(gpa);
        //
        // const res = try cli.fetch(.{
        //     .location = .{ .url = arg[0].obj.type.string.items },
        //     .response_storage = .{ .dynamic = &string },
        //     .max_append_size = std.math.maxInt(usize),
        // });
        //
        // if (res.status != .ok) {
        //     string.deinit(gpa);
        //     return .null;
        // }
        //
        // const obj = try gc.allocObject(gpa, vm, .{ .string = string });
        // return .{ .obj = obj };
        return .null;
    }

    pub fn complex(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 2) return vm.newError(gpa, "Argument Mismatch; expect 1 or 2 got {}", .{len});

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

    pub fn abs(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len != 1) return vm.newError(gpa, "Argument Mismatch; expect 1 got {}", .{len});

        return switch (arg[0]) {
            .integer => |i| .{ .integer = @intCast(@abs(i)) },
            .float => |i| .{ .float = @abs(i) },
            .complex => |z| .{ .float = @sqrt(z.real * z.real + z.imag * z.imag) },
            else => |t| vm.newError(gpa, "Type Mismatch; expect integer or float, got {s}", .{t.name()}),
        };
    }

    pub fn parse(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len != 2) return vm.newError(gpa, "Argument Mismatch; expect 2", .{});

        if (arg[0] != .tag) {
            return vm.newError(gpa, "Expect type/tag, found {s}", .{arg[0].name()});
        }

        const tag = arg[0].tag;
        const ty = tagtype.get(tag) orelse return vm.newError(gpa, "Invalid Type '{s}'", .{tag});

        const string = arg[1].obj.type.string.items;

        return switch (ty) {
            .int => .{ .integer = try std.fmt.parseInt(i32, string, 10) },
            .float => .{ .float = try std.fmt.parseFloat(f32, string) },
            else => vm.newError(gpa, "Invalid type {}", .{ty}),
        };
    }

    pub fn new(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        const len = arg.len;
        if (len == 0 or len > 3) return vm.newError(gpa, "Argument Mismatch; expect at least 3", .{});

        if (arg[0] != .tag) {
            return vm.newError(gpa, "Expect type/tag, found {s}", .{arg[0].name()});
        }

        const tag = arg[0].tag;
        const ty = tagtype.get(tag) orelse return vm.newError(gpa, "Invalid Type '{s}'", .{tag});
        var default = defaultValue(ty);
        const capacity: usize = if (len == 2 or len == 3) @intCast(arg[1].integer) else return default;

        var array = try std.ArrayList(Value).initCapacity(gpa, capacity);
        errdefer array.deinit(gpa);

        if (arg.len == 3) default = arg[2];
        for (0..capacity) |_| try array.append(gpa, default);

        const obj = try gc.allocObject(gpa, vm, .{ .array = array });
        return .{ .obj = obj };
    }

    pub fn import(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        _ = gpa; // autofix
        _ = vm; // autofix
        _ = arg; // autofix
        // const filename = arg[0].obj.type.string.items;
        // var l: @import("Lexer.zig") = .init(filename);
        // var p: @import("Parser.zig") = .init(gpa, &l, vm.errors);
        // defer p.deinit();
        // const node = try p.parse();
        // _ = node;
        // errdefer std.debug.print("-->> {s}\n\n", .{vm.compiler.errors.msg.items});
        // errdefer std.debug.print("-->> {s}\n\n", .{vm.errors.msg.items});
        // try vm.compiler.compile(node);
        // const code = try vm.compiler.bytecode();
        // // defer code.deinit;
        // vm.constants = try std.mem.concat(gpa, Value, &.{ vm.constants, code.constants });
        // vm.positions = try std.mem.concat(gpa, usize, &.{ vm.positions, code.positions });
        return .null;
    }

    pub fn panic(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError(gpa, "Argument Mismatch; expect 1, got {}", .{arg.len});
        if (arg[0] != .obj) return vm.newError(gpa, "Invalid Argument type {s}", .{arg[0].name()});
        return vm.newError(gpa, "panic: {s}", .{arg[0].obj.type.string.items});
    }

    pub fn typeOf(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError(gpa, "Argument Mismatch; expect 1, got {}", .{arg.len});

        return switch (arg[0]) {
            .obj => |ob| switch (ob.type) {
                .instance => |i| .{ .tag = i.type.name orelse "instance" },
                else => .{ .tag = @tagName(ob.type) },
            },
            else => .{ .tag = @tagName(arg[0]) },
        };
    }

    pub fn as(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 2) return vm.newError(gpa, "Argument Mismatch; expect 2, got {}", .{arg.len});

        const ty = tagtype.get(arg[0].tag) orelse return vm.newError(gpa, "Invalid Type", .{});
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

        return vm.newError(gpa, "Invalid {s} to Char coercion", .{value.name()});
    }

    pub fn asChar(gpa: Allocator, vm: *Vm, arg: []const Value) !Value {
        if (arg.len != 1) return vm.newError(gpa, "Argument Mismatch; expect 1, got {}", .{arg.len});

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

        return vm.newError(gpa, "Invalid {s} to Char coercion", .{value.name()});
    }

    pub fn read(gpa: Allocator, vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len == 0) {
            const buff = try gpa.alloc(u8, 1024);
            defer gpa.free(buff);
            // const bytes = try std.fs.File.stdin().reader(buff).readUntilDelimiterAlloc(gpa, '\n', 10000);
            var stdin = std.fs.File.stdin();
            defer stdin.close();
            var theaded: std.Io.Threaded = .init(gpa);
            defer theaded.deinit();
            var r = stdin.reader(theaded.io(), buff);
            const bytes = try r.interface.peekDelimiterExclusive('\n');
            return try newString(gpa, vm, bytes);
        }

        if (arg[0] != .obj) return .null;

        switch (arg[0].obj.type) {
            .string => |str| {
                const file = str.items;
                const bytes = std.fs.cwd().readFileAlloc(file, gpa, .unlimited) catch |err| switch (err) {
                    error.FileNotFound => return .null,
                    else => return err,
                };
                defer gpa.free(bytes);
                // TODO: copy
                return try newString(gpa, vm, bytes);
            },

            else => return .null,
        }
    }

    pub fn write(gpa: Allocator, vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len != 2) return vm.newError(gpa, "Argument Mismatch, expect 2, got {}", .{arg.len});
        if (arg[0] != .obj or arg[1] != .obj) return .null;
        const filename = arg[0].obj.type.string.items;
        const content = arg[1].obj.type.string.items;
        var file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(content);
        return .{ .integer = @intCast(content.len) };
    }

    pub fn length(gpa: Allocator, vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg.len != 1) return vm.newError(gpa, "Argument Mismatch", .{});
        if (arg[0] != .obj) return vm.newError(gpa, "Type '{s}' don't have a length", .{arg[0].name()});
        return switch (arg[0].obj.type) {
            .array => |arr| .{ .integer = @intCast(arr.items.len) },
            .string => |str| .{ .integer = @intCast(str.items.len) },
            .hash => |hash| .{ .integer = @intCast(hash.pairs.count()) },
            else => vm.newError(gpa, "Type '{s}' don't have a length", .{arg[0].name()}),
        };
    }

    pub fn pop(gpa: Allocator, vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg[0] != .obj) return vm.newError(gpa, "Invalid operation: Nothing to append to type '{s}'", .{arg[0].name()});
        switch (arg[0].obj.type) {
            .array => |*arr| {
                return arr.pop() orelse .null;
            },
            else => return .null,
        }
    }

    pub fn append(gpa: Allocator, vm: *Vm, arg: []const Value) anyerror!Value {
        if (arg[0] != .obj) return vm.newError(gpa, "Invalid operation: Nothing to append to type '{s}'", .{arg[0].name()});
        switch (arg[0].obj.type) {
            .array => |*arr| {
                for (arg[1..]) |val| {
                    try arr.append(gpa, val);
                }
            },
            .string => |*str| {
                for (arg[1..]) |val| {
                    if (val == .obj and val.obj.type == .string) {
                        try str.appendSlice(gpa, val.obj.type.string.items);
                        continue;
                    }
                    switch (val) {
                        inline .integer, .float => |x| {
                            var w: std.Io.Writer = .fromArrayList(str);
                            try w.print("{}", .{x});
                        },
                        .char => |c| try str.append(gpa, c),
                        .boolean => |b| try str.appendSlice(gpa, if (b) "true" else "false"),
                        else => {},
                    }
                }
            },
            // .string => |*str| {
            //     for (arg[1..]) |value| {
            //         const string = switch (value) {
            //             .obj => |ob| switch (ob.type) {
            //                 .string => |s| try std.fmt.allocPrint(gpa, "{s}{s}", .{ str.*, s }),
            //                 else => return .null,
            //             },
            //             inline .integer, .float, .boolean => |val| try std.fmt.allocPrint(gpa, "{s}{}", .{ str.*, val }),
            //             .tag => |val| try std.fmt.allocPrint(gpa, "{s}{s}", .{ str.*, val }),
            //             .char => |val| try std.fmt.allocPrint(gpa, "{s}{c}", .{ str.*, val }),
            //             else => return vm.newError(gpa,"Invalid string operation: Cannot concat type '{s}'", .{value.name()}),
            //         };
            //         // const string = std.mem.allocPrint(gpa, "{s}{}", .{ str.*, val }) catch .null;
            //         const obj = try memory.allocateObject(vm, .{ .string = string });
            //         return .{ .obj = obj };
            //     }
            // },
            else => {},
        }
        return .null;
    }

    pub fn print(gpa: Allocator, _: *Vm, args: []const Value) anyerror!Value {
        _ = gpa; // autofix
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
