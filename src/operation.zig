const std = @import("std");
const code = @import("code.zig");
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Vm = @import("Vm.zig");
const builtins = @import("builtins.zig");

/// [], .
pub fn executeIndex(vm: *Vm, left: *const Value, index: *const Value) !void {
    if (left.* == .complex) {
        const z = left.complex;

        if (index.* != .tag) {
            return vm.newError("Invalid Index Operation: complex number is not indexable", .{});
        }

        if (std.mem.eql(u8, index.tag, "real")) {
            return vm.push(.{ .float = z.real });
        }

        if (std.mem.eql(u8, index.tag, "imag")) {
            return vm.push(.{ .float = z.imag });
        }

        return vm.newError("Invalid Index Operation", .{});
    }

    if (left.* != .obj) {
        if (index.* == .tag) {
            return vm.newError("Invalid Index Operation on type '{s}' with index/tag '{s}'", .{ left.name(), index.tag });
        } else {
            return vm.newError("Invalid Index Operation on type '{s}' with index/tag '{s}'", .{ left.name(), index.name() });
        }
    }

    switch (left.obj.type) {
        .namespace => |ns| {
            if (index.* != .tag) {
                return vm.newError("Invalid Index Operation: namespace is not indexable", .{});
            }

            const variable = index.tag;
            if (!ns.map.store.contains(variable)) {
                return vm.newError("variable '{s}' was not declared at namespace {s}", .{ index.tag, ns.name });
            }

            const symbol = ns.map.store.getPtr(variable).?;
            const idx = if (symbol.public) symbol.index else {
                return vm.newError("Symbol '{s}' from namespace '{s}' was not marked as public", .{ symbol.name, ns.name });
            };

            return vm.push(vm.globals[idx].?);
        },
        .instance => |ins| {
            if (index.* != .tag) {
                return vm.newError("Invalid Index Operation: instance is not indexable", .{});
                // return try vm.push(.null);
            }

            if (!ins.fields.contains(index.tag)) {
                return vm.newError("No field named '{s}' in type {s}", .{ index.tag, ins.type.name orelse "anon" });
            }

            const value = ins.fields.get(index.tag) orelse .null;

            // get the
            if (value == .obj and value.obj.type == .decl) {
                value.obj.type.decl.method = left.obj;
            }

            return try vm.push(value);
        },
        .type => |y| {
            // if (y.type != .@"enum") {
            //     return vm.newError("Struct Not Indexable", .{});
            // }
            const value = y.fields.get(index.tag) orelse return vm.newError("no declaration/tag named '{s}' in type {s}", .{ index.tag, y.name orelse "anon" });
            return try vm.push(value);
        },
        .string => |string| {
            if (index.* == .tag) {
                if (std.mem.eql(u8, index.tag, "len")) {
                    return vm.push(.{ .integer = @intCast(string.items.len) });
                }
            }

            if (index.* == .integer) {
                const i = index.integer;
                const len = string.items.len;
                if (i >= 0 and i < len) {
                    const char = string.items[@intCast(i)];
                    return try vm.push(.{ .char = char });
                }
                return vm.newError("Index Out of Bound: string len is {}, index is {}", .{ len, i });
            }

            // TODO: optimize
            if (index.* == .range) {
                var start = index.range.start;
                var end = index.range.end;
                if (end > string.items.len) {
                    end = string.items.len;
                }
                if (start > string.items.len) {
                    start = string.items.len;
                }
                const str = string.items[start..end];
                const value = try builtins.newString(vm, str);
                return try vm.push(value);
            }
        },

        .array => |array| {
            if (index.* == .tag) {
                if (std.mem.eql(u8, index.tag, "len")) {
                    return vm.push(.{ .integer = @intCast(array.items.len) });
                }
            }

            if (index.* == .integer) {
                const i = index.integer;
                const len = array.items.len;
                if (i >= 0 and i < len) {
                    return try vm.push(array.items[@intCast(i)]);
                }

                if (i < 0 and -i < len) {
                    const l: i32 = @intCast(len);
                    return try vm.push(array.items[@intCast(l + i)]);
                }
                return vm.newError("Index Out of Bound: array len is {}, index is {}", .{ len, i });
            }

            // optimize!
            if (index.* == .range) {
                var start = index.range.start;
                var end = index.range.end;
                if (end > array.items.len) {
                    end = array.items.len;
                }
                if (start > array.items.len) {
                    start = array.items.len;
                }
                var arr = try std.ArrayList(Value).initCapacity(vm.allocator, end - start);
                errdefer arr.deinit();
                try arr.appendSlice(array.items[start..end]);
                const obj = try memory.allocateObject(vm, .{ .array = arr });
                return try vm.push(.{ .obj = obj });
            }
        },

        .hash => |hash| {
            const hashkey = try Object.Hash.Key.init(index);
            const pair = hash.pairs.getPtr(hashkey) orelse {
                return vm.push(.null);
            };
            return vm.push(pair.value);
        },

        else => return vm.newError("Invalid Index Operation on type '{s}'", .{left.name()}),
    }

    return try vm.push(.null);
}

pub fn setIndex(vm: *Vm, left: *Value, index: Value, value: Value) !void {
    switch (left.*) {
        .obj => |ob| switch (ob.type) {
            .instance => |*ins| {
                if (index == .tag) {
                    if (!ins.fields.contains(index.tag)) {
                        return vm.newError("No field named '{s}' in type {s}", .{ index.tag, ins.type.name orelse "anon" });
                    }
                    try ins.fields.put(index.tag, value);
                    return vm.push(.null);
                }
                return vm.newError("Invalid Index Operation <3", .{});
            },

            .string => |string| {
                if (index != .integer) return vm.newError("Invalid Index Type", .{});
                if (value != .char) return vm.newError("Invalid String Assigmente, expect char, got {s}", .{value.name()});

                const i = index.integer;
                const len = string.items.len;

                if (i < 0 or i >= len) return vm.newError("Index Out of Bound", .{});

                string.items[@intCast(i)] = value.char;
                return try vm.push(.null);
            },

            .array => |array| {
                if (index != .integer) return vm.newError("Invalid Index Type", .{});

                const i = index.integer;
                const len = array.items.len;

                if (i >= 0 and i < len) {
                    array.items[@intCast(i)] = value;
                    return vm.push(.null);
                }
                return vm.newError("Index Out of Bound", .{});
            },

            .hash => |*hash| {
                const hashkey = try Object.Hash.Key.init(&index);
                const pair = hash.pairs.getPtr(hashkey) orelse {
                    const newpair = Object.Hash.Pair{ .key = index, .value = value };
                    try hash.pairs.put(hashkey, newpair);
                    return vm.push(.null);
                };
                // WARN: potential bug? need to generate a new key?
                pair.value = value;
            },
            else => return vm.newError("Invalid Index Operation: type {s} dont support filed assess", .{left.name()}),
        },

        else => {
            return vm.newError("Invalid Index Operation: type {s} dont support filed assess", .{left.name()});
        },
    }

    return try vm.push(.null);
}

pub fn executePrefix(vm: *Vm, op: code.Opcode) !void {
    const obj = vm.pop();
    switch (obj) {
        .integer => |integer| if (op == .min) return try vm.push(.{ .integer = -integer }),
        .float => |float| if (op == .min) return try vm.push(.{ .float = -float }),
        .boolean => |boolean| if (op == .not) return try vm.push(.{ .boolean = !boolean }),
        .null => if (op == .not) return try vm.push(.null),
        else => {},
    }
    return vm.newError("Invalid Prefix Operation", .{});
}

fn isString(value: *const Value) bool {
    switch (value.*) {
        .obj => |ob| switch (ob.type) {
            .string => return true,
            else => {},
        },
        else => {},
    }

    return false;
}

fn isNumber(v: *Value) bool {
    return if (v.* == .integer or v.* == .float) true else false;
}

/// optimizar essa merda!!!!!!
pub fn executeBinary(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    if (right == .integer and left == .integer) {
        const right_val = right.integer;
        const left_val = left.integer;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            .mod => @mod(left_val, right_val),
            else => unreachable,
        };
        return try vm.push(.{ .integer = result });
    }

    if (right == .long_int and left == .long_int) {
        const right_val = right.long_int;
        const left_val = left.long_int;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            .mod => @mod(left_val, right_val),
            else => unreachable,
        };
        return try vm.push(.{ .long_int = result });
    }

    if (right == .float and left == .integer) {
        const right_val = right.float;
        const left_val: f32 = @floatFromInt(left.integer);
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            .mod => @mod(left_val, right_val),
            else => unreachable,
        };
        return try vm.push(.{ .float = result });
    }

    if (right == .integer and left == .float) {
        const right_val: f32 = @floatFromInt(right.integer);
        const left_val = left.float;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            .mod => @mod(left_val, right_val),
            else => unreachable,
        };
        return try vm.push(.{ .float = result });
    }

    if (left == .boolean and right == .boolean) {
        const left_val = left.boolean;
        const right_val = right.boolean;
        const result = switch (op) {
            .add => left_val and right_val,
            .sub => left_val or right_val,
            else => unreachable,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .float and left == .float) {
        const right_val = right.float;
        const left_val = left.float;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => left_val / right_val,
            else => unreachable,
        };
        return try vm.push(.{ .float = result });
    }

    if (left == .complex and right == .complex) {
        const lval = left.complex;
        const rval = right.complex;
        const complex: Object.Value.Complex = switch (op) {
            .add => .{ .real = lval.real + rval.real, .imag = lval.imag + rval.imag },
            .sub => .{ .real = lval.real - rval.real, .imag = lval.imag - rval.imag },
            .mul => .{
                .real = lval.real * rval.real - lval.imag * rval.imag,
                .imag = lval.real * rval.imag + lval.imag * rval.real,
            },
            else => unreachable,
        };
        return try vm.push(.{ .complex = complex });
    }

    if (left == .float and right == .complex) {
        const scalar = left.float;
        const rval = right.complex;
        const complex: Object.Value.Complex = switch (op) {
            .add => .{ .real = scalar + rval.real, .imag = rval.imag },
            .sub => .{ .real = scalar - rval.real, .imag = -rval.imag },
            .mul => .{ .real = scalar * rval.real, .imag = scalar * rval.imag },
            else => unreachable,
        };
        return try vm.push(.{ .complex = complex });
    }

    if (right == .float and left == .complex) {
        const lval = left.complex;
        const scalar = right.float;
        const complex: Object.Value.Complex = switch (op) {
            .add => .{ .real = scalar + lval.real, .imag = lval.imag },
            .sub => .{ .real = -scalar + lval.real, .imag = lval.imag },
            .mul => .{ .real = scalar * lval.real, .imag = scalar * lval.imag },
            else => unreachable,
        };
        return try vm.push(.{ .complex = complex });
    }

    if (left == .integer and right == .complex) {
        const scalar: f32 = @floatFromInt(left.integer);
        const rval = right.complex;
        const complex: Object.Value.Complex = switch (op) {
            .add => .{ .real = scalar + rval.real, .imag = rval.imag },
            .sub => .{ .real = scalar - rval.real, .imag = -rval.imag },
            .mul => .{ .real = scalar * rval.real, .imag = scalar * rval.imag },
            else => unreachable,
        };
        return try vm.push(.{ .complex = complex });
    }

    if (right == .integer and left == .complex) {
        const lval = left.complex;
        const scalar: f32 = @floatFromInt(right.integer);
        const complex: Object.Value.Complex = switch (op) {
            .add => .{ .real = scalar + lval.real, .imag = lval.imag },
            .sub => .{ .real = -scalar + lval.real, .imag = lval.imag },
            .mul => .{ .real = scalar * lval.real, .imag = scalar * lval.imag },
            else => unreachable,
        };
        return try vm.push(.{ .complex = complex });
    }

    if (right == .obj and left == .obj) {
        if (right.obj.type == .array and left.obj.type == .array) {
            if (op != .add) {
                return vm.newError("Invalid String Operation", .{});
            }
            const left_val = left.obj.type.array;
            const right_val = right.obj.type.array;
            var array = try std.ArrayList(Value).initCapacity(vm.allocator, left_val.items.len + right_val.items.len);
            errdefer array.deinit();

            try array.appendSlice(left_val.items);
            try array.appendSlice(right_val.items);

            const obj = try memory.allocateObject(vm, .{ .array = array });
            return try vm.push(.{ .obj = obj });
        }
        if (right.obj.type == .string and left.obj.type == .string) {
            const left_val = left.obj.type.string;
            const right_val = right.obj.type.string;
            if (op != .add) {
                return vm.newError("Invalid String Operation", .{});
            }
            var string = try std.ArrayList(u8).initCapacity(vm.allocator, left_val.items.len + right_val.items.len);
            errdefer string.deinit();

            try string.appendSlice(left_val.items);
            try string.appendSlice(right_val.items);

            const obj = try memory.allocateObject(vm, .{ .string = string });
            return try vm.push(.{ .obj = obj });
        }
    }

    return vm.newError("Invalid operation '{s}' between {s} and {s}", .{ @tagName(op), left.name(), right.name() });
}

pub fn executeComparison(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    if (std.meta.Tag(@TypeOf(right)) != std.meta.Tag(@TypeOf(left))) {
        try vm.push(.{ .boolean = if (op == .eq) false else true });
    }

    switch (left) {
        .enumtag => |left_val| switch (right) {
            .enumtag => |right_val| {
                if (!std.mem.eql(u8, left_val.from.name.?, right_val.from.name.?)) {
                    return try vm.push(.{ .boolean = false });
                }
                const result = switch (op) {
                    .eq => left_val.id == right_val.id,
                    .neq => left_val.id != right_val.id,
                    .gt => left_val.id > right_val.id,
                    .gte => left_val.id >= right_val.id,
                    else => return vm.newError("Invalid operation!", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {},
        },
        .boolean => |left_val| switch (right) {
            .boolean => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .land => left_val and right_val,
                    .lor => left_val or right_val,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {},
        },

        .integer => |left_val| switch (right) {
            .integer => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid operation 1", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },

            .char => |right_val| {
                const right_val0: i32 = @intCast(right_val);
                const result = switch (op) {
                    .eq => left_val == right_val0,
                    .neq => left_val != right_val0,
                    .gt => left_val > right_val0,
                    .gte => left_val >= right_val0,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },

            else => {},
        },

        .float => |left_val| switch (right) {
            .float => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {},
        },

        .char => |left_val| switch (right) {
            .char => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },

            .integer => |right_val| {
                const right_val0: u8 = @intCast(right_val);
                const result = switch (op) {
                    .eq => left_val == right_val0,
                    .neq => left_val != right_val0,
                    .gt => left_val > right_val0,
                    .gte => left_val >= right_val0,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {}, //return vm.newError("Invalid operation", .{}),
        },

        .null => switch (right) {
            .null => {
                const result = switch (op) {
                    .eq => right == .null and left == .null,
                    .neq => right != .null or left != .null,
                    else => return vm.newError("Invalid operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {}, //return vm.newError("Invalid operation", .{}),
        },

        .obj => |left_val_obj| switch (right) {
            .obj => |right_val_obj| {
                const right_t = right_val_obj.type;
                const left_t = left_val_obj.type;

                switch (right_t) {
                    .string => |rstr| switch (left_t) {
                        .string => |lstr| {
                            const r = std.mem.eql(u8, lstr.items, rstr.items);
                            return try vm.push(.{ .boolean = if (op == .eq) r else if (op == .neq) !r else false });
                        },
                        else => {},
                    },

                    .array => |rarr| switch (left_t) {
                        .array => |larr| {
                            var r = true;
                            if (rarr.items.len == larr.items.len) {
                                for (larr.items, rarr.items) |le, re| {
                                    if (!std.meta.eql(le, re)) {
                                        r = false;
                                        break;
                                    }
                                }
                            } else {
                                r = false;
                            }
                            return try vm.push(.{ .boolean = if (op == .eq) r else if (op == .neq) !r else false });
                        },
                        else => {},
                    },
                    else => {},
                }
            },
            else => {},
        },
        else => {},
    }

    try vm.push(.{ .boolean = if (op == .eq) false else true });
}
