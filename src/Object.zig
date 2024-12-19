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
    builtin: Builtin,
    obj: *Object,
};

/// heap allocated values
/// this name sucks, i know
pub const Type = union(enum) {
    string: []u8,
    array: []Value,
    /// compilation code
    function: CompiledFn,
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
