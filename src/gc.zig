const std = @import("std");
const c = @cImport(@cInclude("gc.h"));

const Gc = struct {
    fn init() void {
        c.GC_init();
    }

    fn alloc(T: type) *T {
        return @alignCast(@ptrCast(c.GC_MALLOC(@sizeOf(T))));
    }

    fn create(T: type) *T {
        return @alignCast(@ptrCast(c.GC_MALLOC_ATOMIC(@sizeOf(T))));
    }
};

test {
    Gc.init();
    const i = Gc.create(i32);
    _ = i; // autofix
}
