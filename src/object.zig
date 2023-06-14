const std = @import("std");
const ast = @import("ast.zig");

const LolliType = enum {
    integer,
    string,
    array,
    hash,
    boolean,
    null,
    @"return",
    err,
    function,
    builtin,
    builtin_method,
};

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    string: String,
    null: Null,
    array: Array,
    hash: Hash,
    @"return": Return,
    err: Error,
    function: Function,
    builtin: Builtin,
    builtin_method: BuiltinMethod,

    pub fn objType(self: *const Object) LolliType {
        return switch (self.*) {
            inline else => |x| x.obj_type,
        };
    }

    pub fn next(self: *Object) anyerror!NextReturn {
        return switch (self.*) {
            .string => |*s| s.next(),
            .array => |*s| s.next(),
            else => error.NotIterable,
        };
    }

    pub fn reset(self: *Object) anyerror!NextReturn {
        return switch (self.*) {
            .string => |*s| s.reset(),
            .array => |*s| s.reset(),
            else => error.NotIterable,
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
    offset: usize = 0,

    pub fn next(self: *String) NextReturn {
        if (self.offset < self.value.len) {
            self.offset += 1;

            const ch = self.value[self.offset - 1];
            var w = "0".*;
            w[0] = ch;
            const w2 = &w;
            const element = .{ .string = .{ .value = w2 } };
            return .{ .element = element, .index = .{ .integer = .{ .value = @intCast(i64, self.offset - 1) } }, .ok = true };
        }

        return .{ .element = .{ .null = Null{} }, .index = .{ .integer = .{ .value = 0 } }, .ok = false };
    }

    pub fn reset(self: *String) void {
        self.offset = 0;
    }
};

pub const Boolean = struct {
    value: bool,
    obj_type: LolliType = .boolean,
};

const NextReturn = struct { element: Object, index: Object, ok: bool };

pub const Array = struct {
    obj_type: LolliType = .array,
    elements: []Object,
    offset: usize = 0,

    pub fn next(self: *Array) NextReturn {
        if (self.offset < self.elements.len) {
            self.offset += 1;

            const element = self.elements[self.offset - 1];
            return .{ .element = element, .index = .{ .integer = .{ .value = @intCast(i64, self.offset - 1) } }, .ok = true };
        }
        return .{ .element = .{ .null = Null{} }, .index = .{ .integer = .{ .value = 0 } }, .ok = false };
    }

    pub fn reset(self: *Array) void {
        self.offset = 0;
    }
};

pub const Hash = struct {
    obj_type: LolliType = .hash,
    elements: std.AutoHashMap(*Object, *Object),
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

pub const BuiltinMethod = struct {
    obj_type: LolliType = .builtin_method,
    method_name: ast.Identifier,
};

pub const Environment = struct {
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Object),
    is_const: std.StringHashMap(bool),
    outer: ?*Environment = null,
    permit: ?[]const []const u8 = null,
    allocated_obj: std.ArrayList([]Object),
    allocated_str: std.ArrayList([]u8),
    allocated_hash: std.ArrayList(std.AutoHashMap(*Object, *Object)),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .store = std.StringHashMap(Object).init(allocator),
            .is_const = std.StringHashMap(bool).init(allocator),
            .allocated_obj = std.ArrayList([]Object).init(allocator),
            .allocated_str = std.ArrayList([]u8).init(allocator),
            .allocated_hash = std.ArrayList(std.AutoHashMap(*Object, *Object)).init(allocator),
        };
    }

    pub fn newEncloseEnv(outer: *Environment) Environment {
        var env = Environment.init(outer.allocator);
        env.outer = outer;
        return env;
    }

    pub fn newTemporaryScope(outer: *Environment, keys: []const []const u8) Environment {
        var env = Environment.init(outer.allocator);
        env.outer = outer;
        env.permit = keys;
        return env;
    }

    pub fn deinit(self: *Environment) void {
        for (self.allocated_str.items) |item| {
            self.allocator.free(item);
        }

        for (self.allocated_obj.items) |item| {
            self.allocator.free(item);
        }

        for (self.allocated_hash.items) |*item| {
            item.deinit();
        }

        self.allocated_str.deinit();
        self.allocated_obj.deinit();
        self.store.deinit();
        self.is_const.deinit();
        self.allocated_hash.deinit();
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

    pub fn dropScopeVar(self: *Environment, name: []const u8) void {
        _ = self.is_const.remove(name);
        _ = self.store.remove(name);
    }

    pub fn setConst(self: *Environment, name: []const u8, obj: Object) !Object {
        if (self.is_const.get(name)) |_| return error.CanNotReassingConstVariables;
        try self.store.put(name, obj);
        try self.is_const.put(name, true);
        return obj;
    }
};
