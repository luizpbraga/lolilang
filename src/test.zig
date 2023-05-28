const std = @import("std");

const U = union(enum) {
    a: A,
    b: B,
};

const A = struct {};
const B = union(enum) { u: *U };

pub fn main() !void {
    // code
    var b = B{ .u = .{.a} };
}
