const std = @import("std");
const code = @import("code.zig");
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Vm = @import("Vm.zig");

/// [], .
pub fn executeIndex(vm: *Vm, left: *const Value, index: *const Value) !void {
    if (left.* != .obj) {
        return vm.newError("Invalid Index Operation", .{});
    }

    switch (left.obj.type) {
        .instance => |ins| {
            if (index.* != .tag) {
                return vm.newError("Invalid Index Operation", .{});
                // return try vm.push(.null);
            }
            const value = ins.fields.get(index.tag) orelse .null;

            if (value == .obj and value.obj.type == .desc) {
                value.obj.type.desc.method = left.obj;
            }

            return try vm.push(value);
        },

        .type => |y| {
            // if (y.type != .@"enum") {
            //     return vm.newError("Struct Not Indexable", .{});
            // }
            const value = y.fields.get(index.tag) orelse return vm.newError("Undefined Struct/Enum Declaration", .{});
            return try vm.push(value);
        },

        .string => |string| {
            if (index.* == .integer) {
                const i = index.integer;
                const len = string.len;
                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    return try vm.push(.{ .char = char });
                }
                return vm.newError("Index Out of Bound", .{});
            }

            // TODO: optimize
            if (index.* == .range) {
                var start = index.range.start;
                var end = index.range.end;
                if (end > string.len) {
                    end = string.len;
                }
                if (start > string.len) {
                    start = string.len;
                }
                const str = try vm.allocator.dupe(u8, string[start..end]);
                const obj = try memory.allocateObject(vm, .{ .string = str });
                return try vm.push(.{ .obj = obj });
            }
        },

        .array => |array| {
            if (index.* == .integer) {
                const i = index.integer;
                const len = array.items.len;
                if (i >= 0 and i < len) {
                    return try vm.push(array.items[@intCast(i)]);
                }
                return vm.newError("Index Out of Bound", .{});
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

        else => return vm.newError("Invalid Index Operation", .{}),
    }

    return try vm.push(.null);
}

pub fn setIndex(vm: *Vm, left: *Value, index: Value, value: Value) !void {
    switch (left.*) {
        .obj => |ob| switch (ob.type) {
            .instance => |*ins| {
                if (index == .tag) {
                    return try ins.fields.put(index.tag, value);
                }
                return vm.newError("Invalid Index Operation", .{});
            },

            .string => |string| {
                if (index != .integer) return vm.newError("Invalid Index Type", .{});

                const i = index.integer;
                const len = string.len;

                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    return try vm.push(.{ .char = char });
                }
                return vm.newError("Index Out of Bound", .{});
            },

            .array => |array| {
                if (index != .integer) return vm.newError("Invalid Index Type", .{});

                const i = index.integer;
                const len = array.items.len;

                if (i >= 0 and i < len) {
                    array.items[@intCast(i)] = value;
                    return;
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
            else => return vm.newError("Invalid Index Operation", .{}),
        },

        else => {
            return vm.newError("Invalid Index Operation", .{});
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

    if (right == .obj and left == .obj) {
        if (right.obj.type == .string and left.obj.type == .string) {
            const left_val = left.obj.type.string;
            const right_val = right.obj.type.string;
            if (op != .add) {
                return vm.newError("Invalid String Operation", .{});
            }
            const string = try std.mem.concat(vm.allocator, u8, &.{ left_val, right_val });
            const obj = try memory.allocateObject(vm, .{ .string = string });
            return try vm.push(.{ .obj = obj });
        }
    }

    return vm.newError("Invalid Operation '{s}' between {s} and {s}", .{ @tagName(op), left.name(), right.name() });
}

pub fn executeComparison(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    if (std.meta.Tag(@TypeOf(right)) != std.meta.Tag(@TypeOf(left))) {
        try vm.push(.{ .boolean = if (op == .eq) false else true });
    }

    switch (left) {
        .boolean => |left_val| switch (right) {
            .boolean => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .land => left_val and right_val,
                    .lor => left_val or right_val,
                    else => return vm.newError("Invalid Operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => return vm.newError("Invalid Operation", .{}),
        },

        .integer => |left_val| switch (right) {
            .integer => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid Operation", .{}),
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
                    else => return vm.newError("Invalid Operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },

            else => {
                return vm.newError("Invalid Operation", .{});
            },
        },

        .float => |left_val| switch (right) {
            .float => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid Operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {
                return vm.newError("Invalid Operation", .{});
            },
        },

        .char => |left_val| switch (right) {
            .char => |right_val| {
                const result = switch (op) {
                    .eq => left_val == right_val,
                    .neq => left_val != right_val,
                    .gt => left_val > right_val,
                    .gte => left_val >= right_val,
                    else => return vm.newError("Invalid Operation", .{}),
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
                    else => return vm.newError("Invalid Operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => {
                return vm.newError("Invalid Operation", .{});
            },
        },

        .null => switch (right) {
            .null => {
                const result = switch (op) {
                    .eq => right == .null and left == .null,
                    .neq => right != .null or left != .null,
                    else => return vm.newError("Invalid Operation", .{}),
                };
                return try vm.push(.{ .boolean = result });
            },
            else => return vm.newError("Invalid Operation", .{}),
        },

        .obj => |left_val_obj| switch (right) {
            .obj => |right_val_obj| {
                const right_t = right_val_obj.type;
                const left_t = left_val_obj.type;

                switch (right_t) {
                    .string => |rstr| switch (left_t) {
                        .string => |lstr| {
                            const r = std.mem.eql(u8, lstr, rstr);
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
