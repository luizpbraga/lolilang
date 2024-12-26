const std = @import("std");
const code = @import("code.zig");
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const Vm = @import("Vm.zig");

pub fn executeIndex(vm: *Vm, left: Value, index: Value) !void {
    switch (left) {
        .obj => |ob| switch (ob.type) {
            .string => |string| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = string.len;

                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    const str = try vm.allocator.dupe(u8, &.{char});
                    const obj = try memory.allocateObject(vm, .{ .string = str });
                    return try vm.push(.{ .obj = obj });
                }
            },

            .array => |array| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = array.len;

                if (i >= 0 and i < len) {
                    return try vm.push(array[@intCast(i)]);
                }
            },

            .hash => |hash| {
                const hashkey = try Object.Hash.Key.init(&index);
                const pair = hash.pairs.getPtr(hashkey) orelse {
                    return vm.push(.null);
                };
                return vm.push(pair.value);
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

pub fn setIndex(vm: *Vm, left: *Value, index: Value, value: Value) !void {
    switch (left.*) {
        .obj => |ob| switch (ob.type) {
            .string => |string| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = string.len;

                if (i >= 0 and i < len) {
                    const char = string[@intCast(i)];
                    const str = try vm.allocator.dupe(u8, &.{char});
                    const obj = try memory.allocateObject(vm, .{ .string = str });
                    return try vm.push(.{ .obj = obj });
                }
            },

            .array => |array| {
                if (index != .integer) return error.InvalidIndexType;

                const i = index.integer;
                const len = array.len;

                if (i >= 0 and i < len) {
                    array[@intCast(i)] = value;
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
            else => unreachable,
        };
        return try vm.push(.{ .integer = result });
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
            else => return error.UnknowfloatOperation,
        };
        return try vm.push(.{ .boolean = result });
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

    return error.UnsupportedOperation;
}
