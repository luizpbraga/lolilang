const std = @import("std");

const LolliType = enum { integer, boolean, null };

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null: Null,

    pub fn objType(self: *const Object) LolliType {
        return switch (self.*) {
            inline else => |x| x.obj_type,
        };
    }

    pub fn inspect(self: *const Object) ![]const u8 {
        var buff: [50]u8 = undefined;
        return switch (self.*) {
            .integer => |obj_type| try std.fmt.bufPrint(&buff, "{d}", .{obj_type.value}),
            .boolean => |obj_type| if (obj_type.value) "true" else "false",
            else => "null",
        };
    }
};

pub const Integer = struct {
    value: i64,
    obj_type: LolliType = .integer,
};

pub const Boolean = struct {
    value: bool,
    obj_type: LolliType = .boolean,
};

pub const Null = struct {
    value: @TypeOf(null) = null,
    obj_type: LolliType = .null,
};
