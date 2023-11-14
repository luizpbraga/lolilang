const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");

pub const NULL = Object{ .null = Null{} };

const LolliType = enum { @"enum", enum_tag, enum_pair, float, integer, string, array, hash, hash_key, hash_pair, boolean, null, @"return", err, function, builtin, builtin_method };

pub const Float = struct {
    value: f64,
    obj_type: LolliType = .float,
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
            return .{ .element = element, .index = .{ .integer = .{ .value = @as(i64, @intCast(self.offset - 1)) } }, .ok = true };
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
            return .{ .element = element, .index = .{ .integer = .{ .value = @as(i64, @intCast(self.offset - 1)) } }, .ok = true };
        }
        return .{ .element = .{ .null = Null{} }, .index = .{ .integer = .{ .value = 0 } }, .ok = false };
    }

    pub fn reset(self: *Array) void {
        self.offset = 0;
    }
};

pub const Hash = struct {
    obj_type: LolliType = .hash,
    pairs: std.AutoHashMap(Key, Pair),

    pub const Key = struct {
        obj_type: LolliType = .hash_key,
        value: usize,

        pub fn init(obj: *const Object) !Key {
            return switch (obj.*) {
                .boolean => |b| .{ .value = if (b.value) 1 else 0 },
                .string => |s| str: {
                    var value: usize = 0;
                    for (s.value) |ch| value += @intCast(ch);
                    break :str .{ .value = value };
                },
                .integer => |i| .{ .value = @intCast(i.value) },

                .builtin_method => |s| str: {
                    var value: usize = 0;
                    for (s.method_name.value) |ch| value += @intCast(ch);
                    break :str .{ .value = value };
                },
                else => error.ObjectCanNotBeAHashKey,
            };
        }
    };

    pub const Pair = struct {
        obj_type: LolliType = .hash_pair,
        key: Object,
        value: Object,
    };
};

pub const Enum = struct {
    obj_type: LolliType = .@"enum",
    tags: std.StringHashMap(Tag),

    pub const Tag = struct {
        obj_type: LolliType = .enum_tag,
        name: []const u8,
        value: *const Object,
        enum_name: ?[]const u8 = undefined,
    };
};

pub const Null = struct {
    // NOTE: this fix the error "union depend on itself" (something like that)
    comptime value: @TypeOf(null) = null,
    obj_type: LolliType = .null,
};

pub const Return = struct {
    /// nao sei pq nao consigo inicializer esse cara
    value: *const Object,
    obj_type: LolliType = .@"return",
};

/// TODO
pub const Error = struct {
    message: []const u8,
    obj_type: LolliType = .err,
};

pub const Function = struct {
    env: *Environment,
    body: ast.BlockStatement,
    parameters: []ast.Identifier,
    obj_type: LolliType = .function,

    fn extendFunctionEnv(func: *Function, args: []Object) anyerror!Environment {
        // TODO: sipa tem que alocar env
        // TODO asset local variables names with args and global variables names
        var enclose_env = func.env.newEncloseEnv();

        for (func.parameters, args) |param, arg|
            _ = try enclose_env.set(param.value, arg);

        return enclose_env;
    }
};

const BuiltinFunction = *const fn ([]const Object) Object;

pub const Builtin = struct {
    func: BuiltinFunction,
    obj_type: LolliType = .builtin,
};

pub const BuiltinMethod = struct {
    obj_type: LolliType = .builtin_method,
    method_name: ast.Identifier,
};

pub const Object = union(enum) {
    err: Error,
    null: Null,
    hash: Hash,
    array: Array,
    string: String,
    integer: Integer,
    float: Float,
    builtin: Builtin,
    boolean: Boolean,
    @"return": Return,
    function: Function,
    enumerator: Enum,
    enum_tag: Enum.Tag,
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

    pub fn applyMethod(obj: *const Object, arg: *const BuiltinMethod) !Object {
        // TODO:
        if (obj.objType() == .hash) {
            return evalHashIndexExpression(obj, &.{ .builtin_method = arg.* });
        }

        if (obj.objType() == .@"enum") {
            return evalEnumIndexExpression(obj, &.{ .builtin_method = arg.* });
        }

        // only string and lists (for now)
        if (std.mem.eql(u8, arg.method_name.value, "len")) {
            const len = switch (obj.objType()) {
                .string => obj.string.value.len,
                .array => obj.array.elements.len,
                .hash => obj.hash.pairs.count(),
                else => return error.MethodLenNotDefined,
            };
            return .{ .integer = .{ .value = @intCast(len) } };
        }

        return error.MethodNotDefined;
    }

    pub fn applyFunction(func: *Object, args: []Object) !Object {
        // TODO 147
        return switch (func.*) {
            .function => |*f| b: {
                var extended_env = try f.extendFunctionEnv(args);
                defer extended_env.deinit();
                var evaluated = try extended_env.eval(.{ .statement = .{ .block_statement = f.body } });
                break :b evaluated.unwrapReturnValue();
            },

            .builtin => |b| b.func(args),

            else => error.NotAFunction,
        };
    }

    pub fn unwrapReturnValue(obj: *const Object) Object {
        if (obj.* == .@"return") {
            return obj.@"return".value.*;
        }
        return obj.*;
    }

    pub fn evalPrefixExpression(right: Object, op: []const u8) Object {
        // TODO: op deve ser um u8
        if (right.objType() == .boolean and op[0] == '!') {
            return .{ .boolean = .{ .value = !right.boolean.value } };
        }

        if (right.objType() == .integer and op[0] == '-') {
            return .{ .integer = .{ .value = -right.integer.value } };
        }

        return NULL;
    }

    pub fn evalIndexExpression(left: *const Object, index: *const Object) !Object {
        return if (left.objType() == .array and index.objType() == .integer)
            evalArrayIndexExpression(left, index)
        else if (left.objType() == .hash)
            evalHashIndexExpression(left, index)
        else
            error.IndexOPNotSupported;
    }

    pub fn evalArrayIndexExpression(array: *const Object, index: *const Object) !Object {
        const arr_obj = array.array;

        const idx = @as(usize, @intCast(index.integer.value));

        const max = arr_obj.elements.len - 1;

        if (idx < 0 or idx > max) {
            return NULL;
        }

        return arr_obj.elements[idx];
    }

    pub fn evalEnumIndexExpression(enum_obj: *const Object, tag_obj: *const Object) Object {
        const tag_name = tag_obj.builtin_method.method_name.value;
        // tag
        const tag = enum_obj.enumerator.tags.get(tag_name) orelse return NULL;

        return .{ .enum_tag = tag };
    }

    pub fn evalHashIndexExpression(hash_obj: *const Object, key_obj: *const Object) !Object {
        const hash = hash_obj.hash;

        const key = try Hash.Key.init(key_obj);

        const pair = hash.pairs.get(key) orelse return NULL;

        return pair.value;
    }
};
