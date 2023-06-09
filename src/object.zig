const std = @import("std");
const ast = @import("ast.zig");

const LolliType = enum { integer, string, array, boolean, null, @"return", err, function, builtin };

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    string: String,
    null: Null,
    array: Array,
    @"return": Return,
    err: Error,
    function: Function,
    builtin: Builtin,

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

pub const String = struct {
    value: []const u8,
    obj_type: LolliType = .string,
};

pub const Boolean = struct {
    value: bool,
    obj_type: LolliType = .boolean,
};

pub const Array = struct {
    obj_type: LolliType = .array,
    elements: []Object,
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
    body: ast.BlockStatement,
    env: *Environment,
};

const BuiltinFunction = *const fn ([]const Object) Object;

pub const Builtin = struct {
    obj_type: LolliType = .builtin,
    func: BuiltinFunction,
};

pub const Environment = struct {
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Object),
    is_const: std.StringHashMap(bool),
    outer: ?*Environment = null,
    allocated_obj: std.ArrayList([]Object),
    allocated_str: std.ArrayList([]u8),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .store = std.StringHashMap(Object).init(allocator),
            .is_const = std.StringHashMap(bool).init(allocator),
            .allocated_obj = std.ArrayList([]Object).init(allocator),
            .allocated_str = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn newEncloseEnv(outer: *Environment) Environment {
        var env = Environment.init(outer.allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinit(self: *Environment) void {
        for (self.allocated_str.items) |item| {
            self.allocator.free(item);
        }

        for (self.allocated_obj.items) |item| {
            self.allocator.free(item);
        }
        self.allocated_str.deinit();
        self.allocated_obj.deinit();
        self.store.deinit();
        self.is_const.deinit();
    }

    pub fn get(self: *Environment, name: []const u8) ?Object {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, name: []const u8, obj: Object) !Object {
        if (self.is_const.get(name)) |is_const| {
            if (is_const) return error.CanNotReassingConstVariables;
        }
        try self.store.put(name, obj);
        try self.is_const.put(name, false);
        return obj;
    }

    pub fn setConst(self: *Environment, name: []const u8, obj: Object) !Object {
        if (self.is_const.get(name)) |_| return error.CanNotReassingConstVariables;
        try self.store.put(name, obj);
        try self.is_const.put(name, true);
        return obj;
    }
};
