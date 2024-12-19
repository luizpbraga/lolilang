const Object = @import("Object.zig");
const Frame = @This();

/// function Instructions
func: Object.CompiledFn,
/// instruction pointer
ip: isize = -1,
/// base pointer: previous ip location before a function call
bp: isize = -1,

pub fn init(func: Object.CompiledFn, bp: isize) Frame {
    return .{ .func = func, .ip = -1, .bp = bp };
}

pub fn instructions(f: *Frame) []u8 {
    return f.func.instructions;
}
