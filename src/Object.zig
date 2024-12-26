const std = @import("std");
const code = @import("code.zig");

/// allocated objects
pub const Object = @This();

type: Type,
next: ?*Object = null,
marked: bool = false,

pub const Value = union(enum) {
    null,
    boolean: bool,
    integer: i32,
    float: f32,
    char: u8,
    builtin: Builtin,
    obj: *Object,
    range: Range,

    pub fn toRange(val: *const Value) Range {
        var end: usize = 0;
        var value: *const Value = &.null;

        switch (val.*) {
            .integer => |int| {
                end = @intCast(int);
                value = val;
            },

            .obj => |ob| switch (ob.type) {
                .array => |array| {
                    end = array.len;
                    value = val;
                },

                .hash => |hash| {
                    end = hash.pairs.count();
                    value = val;
                },

                .string => |str| {
                    end = str.len;
                    value = val;
                },

                else => {},
            },

            else => {},
        }

        return .{ .end = end, .start = 0, .value = value };
    }
};

pub const Range = struct {
    value: *const Value,
    start: usize,
    end: usize,

    pub fn next(r: *Range) ?Value {
        switch (r.value.*) {
            else => {},

            .integer => {
                if (r.start < r.end) {
                    defer r.start += 1;
                    return .{ .integer = @intCast(r.start) };
                }
            },

            .obj => |ob| switch (ob.type) {
                .array => |array| {
                    if (r.start < r.end) {
                        defer r.start += 1;
                        return array[r.start];
                    }
                },

                .string => |string| {
                    if (r.start < r.end) {
                        defer r.start += 1;
                        return .{ .char = string[r.start] };
                    }
                },

                // .hash => |hash| {
                //     if (r.start < r.end) {
                //         defer r.start += 1;
                //         hash.pairs.iterator();
                //     }
                // },

                else => {},
            },
        }

        return null;
    }
};

test {
    var value: Value = .null;
    var range = value.toRange();
    try std.testing.expect(range.next() == null);
}

test {
    var value: Value = .{ .integer = 5 };
    var range = value.toRange();
    try std.testing.expect(range.next().?.integer == 0);
    try std.testing.expect(range.next().?.integer == 1);
    try std.testing.expect(range.next().?.integer == 2);
    try std.testing.expect(range.next().?.integer == 3);
    try std.testing.expect(range.next().?.integer == 4);
    try std.testing.expect(range.next() == null);
}

test {
    const talloc = std.testing.allocator;
    const obj = try talloc.create(Object);
    defer talloc.destroy(obj);

    var array: [5]Value = undefined;
    for (0..5) |i| {
        array[i] = .{ .integer = @intCast(i) };
    }
    obj.type = .{ .array = &array };

    var value: Value = .{ .obj = obj };
    var range = value.toRange();
    try std.testing.expect(range.next().?.integer == 0);
    try std.testing.expect(range.next().?.integer == 1);
    try std.testing.expect(range.next().?.integer == 2);
    try std.testing.expect(range.next().?.integer == 3);
    try std.testing.expect(range.next().?.integer == 4);
    try std.testing.expect(range.next() == null);
}

fn arrayToRange(value: *Value) Range {
    return .{
        .value = value,
        .start = 0,
        .end = value.obj.type.array.len,
    };
}

/// heap allocated values
/// this name sucks, i know
pub const Type = union(enum) {
    string: []u8,
    array: []Value,
    /// compilation code
    function: CompiledFn,
    hash: Hash,
};

pub const CompiledFn = struct {
    /// function bytecode
    instructions: code.Instructions,
    /// number of local bindings
    num_locals: usize = 0,
    num_parameters: usize = 0,
};

pub const Builtin = struct {
    name: []const u8,
    function: *const fn ([]const Value) Value,
};

pub const Hash = struct {
    pairs: std.AutoArrayHashMap(Key, Pair),

    pub const Pair = struct {
        key: Value,
        value: Value,
    };

    pub const Key = struct {
        value: usize,

        pub fn init(val: *const Value) !Key {
            return switch (val.*) {
                .boolean => |b| .{ .value = if (b) 1 else 0 },

                .integer => |i| .{ .value = @intCast(i) },
                .char => |i| .{ .value = @intCast(i) },

                .obj => |ob| switch (ob.type) {
                    .string => |string| str: {
                        var value: usize = 0;
                        for (string) |ch| value += @intCast(ch);
                        break :str .{ .value = value };
                    },
                    else => error.ObjectCanNotBeAHashKey,
                },

                else => error.ObjectCanNotBeAHashKey,
            };
        }
    };
};
