const Object = @import("Object.zig");
const Frame = @This();

/// function Instructions
cl: Object.Closure,
/// instruction pointer
ip: isize = -1,
/// base pointer: previous ip location before a function call
bp: isize = -1,

pub fn init(cl: Object.Closure, bp: isize) Frame {
    return .{ .cl = cl, .ip = -1, .bp = bp };
}

pub fn instructions(f: *Frame) []u8 {
    return f.cl.func.instructions;
}
