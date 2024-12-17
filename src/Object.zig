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
    integer: i64,
    float: f64,
    obj: *Object,
};

/// heap allocated values
/// this name sucks, i know
pub const Type = union(enum) {
    string: []u8,
    array: []Value,
    /// compilation code
    function: code.Instructions,
};
