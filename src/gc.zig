const Vm = @import("Vm.zig");
const std = @import("std");
const Object = @import("Object.zig");
const Compiler = @import("Compiler.zig");
const Allocator = std.mem.Allocator;

pub fn allocObject(gpa: Allocator, vm: *Vm, tag: Object.Type) !*Object {
    const obj = try gpa.create(Object);
    errdefer gpa.destroy(obj);
    vm.bytes_allocated += @sizeOf(*Object);
    // if (vm.bytes_allocated > ) {
    if (vm.bytes_allocated > Vm.GC_MAX) {
        try collectGarbage(gpa, vm);
        // TODO
        // vm.t = std.time.timestamp() - vm.t;
        // if (vm.t < 60) {
        //     Vm.GC_MAX = 5 * Vm.GC_MAX;
        //     vm.t = std.time.timestamp();
        // }
    }
    obj.type = tag;
    obj.marked = false;
    obj.next = vm.objects;
    vm.objects = obj;
    return obj;
}

pub fn collectGarbage(gpa: Allocator, vm: *Vm) !void {
    try markRoots(gpa, vm);
    try traceReference(gpa, vm);
    sweep(gpa, vm);
}

pub fn sweep(gpa: Allocator, vm: *Vm) void {
    var previous: ?*Object = null;
    var obj: ?*Object = vm.objects;

    while (obj != null) {
        if (obj.?.marked) {
            obj.?.marked = false;
            previous = obj;
            obj = obj.?.next;
            continue;
        }

        const unreached: *Object = obj.?;
        obj = obj.?.next;

        if (previous != null) {
            previous.?.next = obj;
        } else {
            vm.objects = obj;
        }

        freeObject(gpa, vm, unreached);
    }
}

pub fn freeObject(gpa: Allocator, vm: *Vm, obj: *Object) void {
    defer vm.bytes_allocated -= @sizeOf(*Object);
    defer gpa.destroy(obj);
    switch (obj.type) {
        .string => |*string| {
            // vm.allocator.free(str);
            // vm.allocator.destroy(obj);
            string.deinit(gpa);
        },

        .array => |*array| {
            // vm.allocator.free(array);
            array.deinit(gpa);
        },

        .hash => |*hash| {
            hash.pairs.deinit();
            // vm.allocator.free(hash);
        },

        .function, .decl => |func| {
            gpa.free(func.instructions);
        },

        .closure => |cl| {
            gpa.free(cl.func.instructions);
            gpa.free(cl.free);
        },

        .type => |*ty| {
            ty.fields.deinit();
            if (ty.decl) |*decl| decl.deinit();
        },

        .instance => |*ty| {
            ty.fields.deinit();
        },

        .namespace => |*ns| {
            ns.map.deinit(gpa);
            gpa.destroy(ns.map);
        },
    }
}

pub fn traceReference(gpa: Allocator, vm: *Vm) !void {
    while (vm.gray_count > 0) {
        vm.gray_count -= 1;
        const obj = vm.gray_stack.orderedRemove(vm.gray_count);
        try blackenObject(gpa, vm, obj);
    }
}

pub fn blackenObject(gpa: Allocator, vm: *Vm, obj: *Object) !void {
    switch (obj.type) {
        .array => |array| {
            for (array.items) |element| {
                try markValue(gpa, vm, element);
            }
        },
        else => {},
    }
}

pub fn markRoots(gpa: Allocator, vm: *Vm) !void {
    for (vm.stack) |values| {
        try markValue(gpa, vm, values);
    }

    for (vm.globals) |globals| {
        if (globals) |glob| {
            try markValue(gpa, vm, glob);
        }
    }
}

pub fn markValue(gpa: Allocator, vm: *Vm, value: Object.Value) !void {
    if (value == .obj) {
        return try markObject(gpa, vm, value.obj);
    }
}

// mark heap objects
pub fn markObject(gpa: std.mem.Allocator, vm: *Vm, ob: *Object) !void {
    if (ob.marked) return;
    ob.marked = true;
    try vm.gray_stack.insert(gpa, vm.gray_count, ob);
    vm.gray_count += 1;
}

test {}
