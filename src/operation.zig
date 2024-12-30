const std = @import("std");
const code = @import("code.zig");
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Vm = @import("Vm.zig");

pub fn executeIndex(vm: *Vm, left: *const Value, index: *const Value) !void {
    if (left.* != .obj) {
        std.debug.print("{} {}", .{ left, index });
        return error.InvalidIndexOperation;
    }

    switch (left.obj.type) {
        .string => |string| {
            if (index.* == .integer) {
                const i = index.integer;
                const len = string.len;

                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    const str = try vm.allocator.dupe(u8, &.{char});
                    const obj = try memory.allocateObject(vm, .{ .string = str });
                    return try vm.push(.{ .obj = obj });
                }

                return;
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

        else => return error.InvalidIndexOperation,
    }

    return try vm.push(.null);
}

pub fn setIndex(vm: *Vm, left: *Value, index: Value, value: Value) !void {
    switch (left.*) {
        .obj => |ob| switch (ob.type) {
            .string => |string| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = string.len;

                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    return try vm.push(.{ .char = char });
                }
            },

            .array => |array| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = array.items.len;

                if (i >= 0 and i < len) {
                    array.items[@intCast(i)] = value;
                }
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
            else => return error.InvalidIndexOperation,
        },

        else => {
            std.debug.print("{} {}", .{ left, index });
            return error.InvalidIndexOperation;
        },
    }

    return try vm.push(.null);
}

pub fn executePrefix(vm: *Vm, op: code.Opcode) !void {
    const obj = try vm.pop();

    switch (obj) {
        .integer => |integer| {
            if (op == .min) {
                return try vm.push(.{ .integer = -integer });
            }
            return error.UnknowIntegerOperation;
        },

        .float => |float| {
            if (op == .min) {
                return try vm.push(.{ .float = -float });
            }
            return error.UnknowIntegerOperation;
        },

        .boolean => |boolean| {
            if (op == .not) {
                return try vm.push(.{ .boolean = !boolean });
            }
            return error.UnknowBooleanOperation;
        },

        .null => {
            if (op == .not) {
                return try vm.push(.null);
            }
            return error.UnknowBooleanOperation;
        },

        else => return error.UnsupportedOperation,
    }
}

pub fn executeBinary(vm: *Vm, op: code.Opcode) !void {
    const right = try vm.pop();
    const left = try vm.pop();

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
                return error.UnsupportedStringOperation;
            }
            const string = try std.mem.concat(vm.allocator, u8, &.{ left_val, right_val });
            const obj = try memory.allocateObject(vm, .{ .string = string });
            return try vm.push(.{ .obj = obj });
        }
    }

    std.debug.print("{} {} {}", .{ left, op, right });

    return error.UnsupportedOperation;
}

pub fn executeComparison(vm: *Vm, op: code.Opcode) !void {
    const right = try vm.pop();
    const left = try vm.pop();

    if (right == .boolean and left == .boolean) {
        const right_val = right.boolean;
        const left_val = left.boolean;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .char and left == .char) {
        const right_val = right.char;
        const left_val = left.char;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .null or left == .null) {
        const result = switch (op) {
            .eq => right == .null and left == .null,
            .neq => right != .null or left != .null,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .integer and left == .integer) {
        const right_val = right.integer;
        const left_val = left.integer;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            .gte => left_val >= right_val,
            else => return error.UnknowIntegerOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .float and left == .float) {
        const right_val = right.float;
        const left_val = left.float;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            .gte => left_val >= right_val,
            else => return error.UnknowfloatOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (left == .obj and right == .obj) {
        const right_t = right.obj.type;
        const left_t = left.obj.type;

        if (right_t == .string) {
            switch (left_t) {
                .string => |lstr| {
                    const rstr = right_t.string;
                    const r = std.mem.eql(u8, lstr, rstr);
                    return vm.push(.{ .boolean = if (op == .eq) r else if (op == .neq) !r else false });
                },
                else => {},
            }
        }

        if (right_t == .array) {
            switch (left_t) {
                .array => |larr| {
                    const rarr = right_t.array;
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

                    return vm.push(
                        .{ .boolean = if (op == .eq) r else if (op == .neq) !r else false },
                    );
                },
                else => {},
            }
        }
    }

    // if (rtype == .float and ltype == .integer) {
    //     const right_val = right.float.value;
    //     const left_val = left.integer.value;
    //     const result = switch (op) {
    //         .eq => left_val == right_val,
    //         .neq => left_val != right_val,
    //         .gt => left_val > right_val,
    //         else => return error.UnknowfloatOperation,
    //     };
    //     return try vm.push(.{ .boolean = .{ .value = result } });
    // }
    //
    // if (rtype == .integer and ltype == .float) {
    //     const right_val = right.integer.value;
    //     const left_val = left.float.value;
    //     const result = switch (op) {
    //         .eq => left_val == right_val,
    //         .neq => left_val != right_val,
    //         .gt => left_val > right_val,
    //         else => return error.UnknowfloatOperation,
    //     };
    //     return try vm.push(.{ .boolean = .{ .value = result } });
    // }

    try vm.push(.{ .boolean = if (op == .eq) false else true });
}
