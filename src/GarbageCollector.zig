//! Jokens on me!!!! Fuck it
const std = @import("std");
const object = @import("./object.zig");

pub const GarbageCollector = @This();
const Object = object.Object;
const Map = std.AutoHashMap(*void, Info);

pub const Type = union(enum) {
    string: usize, // []const u8,
    objeto: usize, // []const Object
};

pub const Info = struct { t: Type, ref: usize = 0 };

map: Map,

pub fn put(gc: *GarbageCollector, ptr: anytype, t: Type) !void {
    const p: *void = @ptrCast(ptr);

    var i = gc.map.getPtr(p) orelse {
        return try gc.map.put(p, .{ .t = t });
    };

    i.ref += 1;
}

pub fn incRef(gc: *GarbageCollector, obj: Object) void {
    const p: *void = switch (obj) {
        .string => |it| @ptrCast(@constCast(it.value)),
        .array => |it| @ptrCast(@constCast(it.elements)),
        else => return,
    };

    var info = gc.map.getPtr(p) orelse return;
    info.ref += 1;
}

pub fn contains(gc: *GarbageCollector, ptr: anytype) bool {
    const p: *void = @ptrCast(@constCast(ptr));
    return gc.map.contains(p);
}

pub fn init(allocator: std.mem.Allocator) GarbageCollector {
    return .{
        .map = Map.init(allocator),
    };
}

pub fn deinit(gc: *GarbageCollector) void {
    while (gc.map.count() != 0) {
        gc.free();
    }

    gc.map.deinit();
}

pub fn free(gc: *GarbageCollector) void {
    var it = gc.map.iterator();

    while (it.next()) |ptr| {
        const pval = ptr.value_ptr;

        if (pval.ref > 0) {
            pval.ref -= 1;
            continue;
        }

        const pkey = ptr.key_ptr;

        switch (pval.t) {
            .string => |len| {
                const p = @as([*]u8, @ptrCast(pkey.*))[0..len];
                gc.map.allocator.free(p);
            },

            .objeto => |len| {
                const p = @as([*]Object, @alignCast(@ptrCast(pkey.*)))[0..len];
                gc.map.allocator.free(p);
            },
        }

        _ = gc.map.remove(pkey.*);
    }
}

// fn deinit(gc: *GarbageCollector) void {
//     defer gc.map.deinit();
//
//     while (gc.map.count() != 0) {
//         var it = gc.map.iterator();
//
//         while (it.next()) |ptr| {
//             const pval = ptr.value_ptr;
//
//             if (pval.ref > 0) continue;
//
//             const pkey = ptr.key_ptr;
//
//             switch (pval.t) {
//                 .string => |len| {
//                     const p = @as([*]u8, @ptrCast(pkey.*))[0..len];
//                     gc.map.allocator.free(p);
//                 },
//
//                 .objeto => |len| {
//                     const p = @as([*]Object, @ptrCast(pkey.*))[0..len];
//                     gc.map.allocator.free(p);
//                 },
//             }
//
//             _ = gc.map.remove(pkey.*);
//         }
//     }
// }
