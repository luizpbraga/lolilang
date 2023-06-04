const std = @import("std");
const ast = @import("ast.zig");

const LolliType = enum { integer, boolean, null, @"return", err, function };

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null: Null,
    @"return": Return,
    err: Error,
    function: Function,

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

pub const Return = struct {
    value: *const Object = &.{ .null = .{} },
    obj_type: LolliType = .@"return",
};

/// TODO
pub const Error = struct {
    message: []const u8,
    obj_type: LolliType = .err,
};

pub const Function = struct {
    obj_type: LolliType = .function,
    parameters: []ast.Identifier,
    body: *ast.BlockStatement,
    env: *Environment,
};

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .store = std.StringHashMap(Object).init(allocator),
            .outer = null,
        };
    }

    pub fn newCloseEnv(outer: *Environment) Environment {
        _ = outer;
    }

    pub fn newEnv() Environment {
        // TODO: 145
    }

    pub fn deinit(self: *Environment) void {
        self.store.deinit();
    }

    pub fn get(self: *Environment, name: []const u8) ?Object {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, name: []const u8, obj: Object) !Object {
        try self.store.put(name, obj);
        return obj;
    }
};
