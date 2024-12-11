// const c = @cImport(@cInclude("gc.h"));
// const obj = @import("object.zig");
//
// const Gc = struct {
//     fn init() void {
//         c.GC_init();
//     }
//
//     fn alloc(T: type) *T {
//         return @alignCast(@ptrCast(c.GC_MALLOC(@sizeOf(T))));
//     }
//
//     fn create(T: type, n: usize) []T {
//         return @alignCast(@ptrCast(c.GC_MALLOC(@sizeOf(T) * n)));
//     }
// };

// test {
//     Gc.init();
//     const i = Gc.alloc(obj.Object);
//     i.* = .{ .string = .{ .value = "ola" } };
//     try std.testing.expectEqualStrings(i.string.value, "ola");
//
//     const str = Gc.create(u8, 10);
//
//     str = "1234567890";
//
//     c.GC_gcollect();
//
//     std.debug.print("Bytes alocados: {}\n", .{c.GC_get_heap_size()});
//     std.debug.print("Bytes livres: {}\n", .{c.GC_get_free_bytes()});
// }

const std = @import("std");
const object = @import("object.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
// any object being referenced by a variable still in scope is in use.
// any object referenced by another in-use object is in use.
//
// in the final grath: reachable, else: dead

const STACK_SIZE = 2040;

var stack: [STACK_SIZE]object.Object = undefined;
var marked: [STACK_SIZE]bool = undefined;

fn markAll(vm: *Vm) void {
    for (0..STACK_SIZE) |i| {
        mark(vm, i);
    }
}

fn mark(vm: *Vm, i: usize) void {
    // if already marked, its done
    if (marked[i]) return;

    marked[i] = true;
    const o = vm.stack[i];

    // marks all objects inside objects , arrays etc
    switch (o) {
        .array => {
            // como eu sei qual o indice do objeto interno ???
            mark(vm, i + 1);
        },
        else => {},
    }
}

const VMObject = struct {
    object: object.Object,
    marked: bool,
};

test {
    var vm: Vm = undefined;
    markAll(&vm);
    // defer vm.deinit();
    // const talloc = std.testing.allocator;
    // var heap: std.ArrayList(*object.Object) = .init(talloc);
    // defer heap.deinit();
    // var compiler = Compiler.init(talloc);
    // defer compiler.deinit();
}
