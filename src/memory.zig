const Vm = @import("Vm.zig");
const std = @import("std");
const Object = @import("Object.zig");
const Compiler = @import("Compiler.zig");

pub fn allocateObject(vm: *Vm, tag: Object.Type) !*Object {
    const allocator = vm.allocator;
    const obj = try allocator.create(Object);
    errdefer allocator.destroy(obj);

    vm.bytes_allocated += @sizeOf(*Object);
    // if (vm.bytes_allocated > ) {
    if (vm.bytes_allocated > Vm.GC_MAX) {
        try collectGarbage(vm);
    }

    obj.type = tag;
    obj.marked = false;

    obj.next = vm.objects;
    vm.objects = obj;

    return obj;
}

pub fn collectGarbage(vm: *Vm) !void {
    try markRoots(vm);
    try traceReference(vm);
    sweep(vm);
}

pub fn sweep(vm: *Vm) void {
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

        freeObject(vm, unreached);
    }
}

pub fn freeObject(vm: *Vm, obj: *Object) void {
    switch (obj.type) {
        .string => |str| {
            vm.allocator.free(str);
            vm.allocator.destroy(obj);
        },

        .array => |array| {
            vm.allocator.free(array);
            vm.allocator.destroy(obj);
        },

        .function => |func| {
            vm.allocator.free(func.instructions);
            vm.allocator.destroy(obj);
        },
    }
}

pub fn traceReference(vm: *Vm) !void {
    while (vm.gray_count > 0) {
        vm.gray_count -= 1;
        const obj = vm.gray_stack.orderedRemove(vm.gray_count);
        try blackenObject(vm, obj);
    }
}

pub fn blackenObject(vm: *Vm, obj: *Object) !void {
    switch (obj.type) {
        .array => |array| {
            for (array) |element| {
                try markValue(vm, element);
            }
        },
        else => {},
    }
}

pub fn markRoots(vm: *Vm) !void {
    for (&vm.stack) |values| {
        try markValue(vm, values);
    }

    for (vm.globals) |globals| {
        if (globals) |glob| {
            try markValue(vm, glob);
        }
    }
}
pub fn markValue(vm: *Vm, value: Object.Value) !void {
    if (value == .obj) {
        return try markObject(vm, value.obj);
    }
}

// mark heap objects
pub fn markObject(vm: *Vm, ob: *Object) !void {
    if (ob.marked) return;

    ob.marked = true;
    try vm.gray_stack.insert(vm.gray_count, ob);
    vm.gray_count += 1;
}

test {}
