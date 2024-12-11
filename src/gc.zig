const std = @import("std");
const c = @cImport(@cInclude("gc.h"));
const obj = @import("object.zig");

const Gc = struct {
    fn init() void {
        c.GC_init();
    }

    fn alloc(T: type) *T {
        return @alignCast(@ptrCast(c.GC_MALLOC(@sizeOf(T))));
    }

    fn create(T: type, n: usize) []T {
        return @alignCast(@ptrCast(c.GC_MALLOC(@sizeOf(T) * n)));
    }
};

test {
    Gc.init();
    const i = Gc.alloc(obj.Object);
    i.* = .{ .string = .{ .value = "ola" } };
    try std.testing.expectEqualStrings(i.string.value, "ola");

    const str = Gc.create(u8, 10);

    str = "1234567890";

    c.GC_gcollect();

    std.debug.print("Bytes alocados: {}\n", .{c.GC_get_heap_size()});
    std.debug.print("Bytes livres: {}\n", .{c.GC_get_free_bytes()});
}
