const std = @import("std");
const ast = @import("ast.zig");
const object = @import("./object.zig");
const buildins = @import("./buildins.zig").buildins;
const GarbageCollector = @import("GarbageCollector.zig");

const eql = std.mem.eql;

const Environment = @This();

gc: *GarbageCollector,
allocator: std.mem.Allocator,

outer: ?*Environment = null,
permit: ?[]const []const u8 = null,
is_const: std.StringHashMap(bool),
store: std.StringHashMap(object.Object),

pub fn init(allocator: std.mem.Allocator, gc: *GarbageCollector) Environment {
    return .{
        .gc = gc,
        .allocator = allocator,
        .store = std.StringHashMap(object.Object).init(allocator),
        .is_const = std.StringHashMap(bool).init(allocator),
    };
}

pub fn deinit(self: *Environment) void {
    self.store.deinit();
    self.is_const.deinit();
    self.gc.free();
}

pub fn newEncloseEnv(outer: *Environment) Environment {
    var env = Environment.init(outer.allocator, outer.gc);
    env.outer = outer;
    return env;
}

pub fn newTemporaryScope(outer: *Environment, keys: []const []const u8) Environment {
    var env = Environment.init(outer.allocator, outer.gc);
    env.outer = outer;
    env.permit = keys;
    return env;
}

pub fn get(env: *Environment, name: []const u8) ?object.Object {
    return env.store.get(name) orelse if (env.outer) |outer| outer.get(name) else null;
}

pub fn getPtr(env: *Environment, name: []const u8) ?*object.Object {
    return env.store.getPtr(name) orelse if (env.outer) |outer| outer.getPtr(name) else null;
}

pub fn _get(env: *Environment, name: []const u8) ?object.Object {
    return env.store.get(name) orelse null;
}

/// is the variable exists, it's main *env is returned
/// BUG: var i = 0 var i = 2 nao deveria ser valido
pub fn setCheck(env: *Environment, name: []const u8) !*Environment {
    // verifica se o env outer possui var com valor const
    const is_const = env.is_const.get(name) orelse false;

    if (is_const) {
        return error.CanNotReassingConstVariables;
    }

    if (env.outer) |outer_env| {
        return try outer_env.setCheck(name);
    }

    return env;
}

pub fn setConst(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    _ = try env.setCheck(name);

    if (env.get(name)) |_| {
        std.log.err("variable {s} exists", .{name});
        return error.CanNotRedeclareExistinVariables;
    }

    try env.store.put(name, obj);
    try env.is_const.put(name, true);
    env.gc.incRef(obj);
    return obj;
}

pub fn setVar(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    if (env.get(name)) |_| {
        return error.VariableCanNotBeRedeclared;
    }

    const env_ = try env.setCheck(name);
    try env_.store.put(name, obj);
    try env_.is_const.put(name, false);
    env.gc.incRef(obj);
    return obj;
}

pub fn assigVar(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    _ = env.get(name) orelse return error.VariableNotDeclared;
    const var_env = try env.setCheck(name);
    env.gc.incRef(obj);
    try var_env.store.put(name, obj);
    return obj;
}

pub fn dropScopeVar(env: *Environment, name: []const u8) void {
    _ = env.is_const.remove(name);
    _ = env.store.remove(name);
}

fn evalIdentifier(env: *Environment, node: *const ast.Identifier) !object.Object {
    if (env.get(node.value)) |val| {
        return val;
    }

    if (env.outer) |outer_env| {
        if (outer_env.get(node.value)) |val|
            return val;
    }

    if (buildins.get(node.value)) |val| {
        return .{ .builtin = val };
    }

    std.log.warn("the identifier: {s}\n", .{node.value});
    return error.IdentifierNotFound;
}

pub fn evalArrayLiteral(env: *Environment, array: *const ast.ArrayLiteral) anyerror!object.Object {
    const obj_list = try env.evalExpressions(array.elements);
    return .{ .array = .{ .elements = obj_list } };
}

pub fn eval(env: *Environment, node: ast.Node) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .null_literal => object.NULL,

                .boolean => |b| .{ .boolean = .{ .value = b.value } },

                .string_literal => |s| .{ .string = .{ .value = s.value } },

                .char_literal => |c| .{ .char = .{ .value = c.value } },

                .integer_literal => |i| .{ .integer = .{ .value = i.value } },

                .float_literal => |f| .{ .float = .{ .value = f.value } },

                .identifier => |*iden| try env.evalIdentifier(iden),

                .array_literal => |*array| try env.evalArrayLiteral(array),

                .type => undefined,

                else => unreachable,

                .range => |*range| try env.evalRange(range),
                //
                // .enum_tag => |et| tag: {
                //     const value = try env.evalIdentifier(&et.value);
                //     break :tag .{ .enum_tag = .{ .value = &value, .name = et.value.value } };
                // },
                // .enum_literal => |*enu| .{ .enumerator = .{ .tags = try env.evalEnumExpression(enu) } },
                .hash_literal => |*hash| .{ .hash = .{ .pairs = try env.evalHashExpression(hash) } },

                .function_literal => |*func| .{ .function = .{ .parameters = func.parameters, .body = func.body, .env = env } },

                .assignment_expression => |ass| try env.evalAssignment(ass),

                .switch_expression => |*swi| try env.evalSwitchExpression(swi),

                .if_expression => |*if_exp| try env.evalIfExpression(if_exp),

                .forloop_expression => |*fl| try env.evalForLoopExpression(fl),

                .forloop_range_expression => |*flr| try env.evalForLoopRangeExpression(flr),

                .multi_forloop_range_expression => @panic(""),

                .prefix_expression => |pre| blk: {
                    const right = try env.eval(.{ .expression = pre.right.* });
                    break :blk right.evalPrefixExpression(pre.operator);
                },

                .infix_expression => |pre| blk: {
                    var left = try env.eval(.{ .expression = pre.left.* });
                    var right = try env.eval(.{ .expression = pre.right.* });
                    break :blk try env.evalInfixExpression(pre.operator, &left, &right);
                },

                .call_expression => |call| blk: {
                    var func = try env.eval(.{ .expression = call.function.* });
                    const args = try env.evalExpressions(call.arguments);
                    break :blk try func.applyFunction(args);
                },

                .index_expression => |idx| blk: {
                    const left = try env.eval(.{ .expression = idx.left.* });
                    const index = try env.eval(.{ .expression = idx.index.* });
                    break :blk left.evalIndexExpression(&index);
                },

                // TODO: generalize it for enum.
                // type_expression
                // .struct_type => |*str| b: {
                //     const struct_type: object.Object = try env.evalStructTypeExpression(str);
                //     break :b try env.setConst(str.name.value, struct_type);
                // },

                .init_expression => |init_exp| {
                    switch (init_exp.type.*) {
                        else => {
                            std.debug.print("{}\n", .{init_exp.type.*});
                            return error.ExpressionMustBeAType;
                        },

                        .identifier => |ident| {
                            const struct_type = env.get(ident.value).?;

                            if (struct_type.objType() != .type) {
                                return error.ExpressionMustBeAType;
                            }

                            // DEFAULT FIELDS
                            const struct_literal_default_fields = struct_type.type.fields;
                            // FIELDS INITIALIZED BY THE USER
                            const struct_literal_fields = &init_exp.struct_;

                            var fields = try env.allocator.create(std.StringHashMap(object.Struct.Field));
                            errdefer env.allocator.destroy(fields);

                            fields.* = std.StringHashMap(object.Struct.Field).init(env.allocator);
                            errdefer fields.deinit();

                            var default_iter = struct_literal_default_fields.valueIterator();
                            while (default_iter.next()) |field_value| {
                                const ident_value = field_value.name;
                                const value = field_value.value;
                                const field: object.Struct.Field = .{ .value = value, .name = ident_value };
                                try fields.put(ident_value, field);
                            }

                            var runtime_iter = struct_literal_fields.fields.iterator();
                            while (runtime_iter.next()) |entry| {
                                const ident_value = entry.key_ptr.*;
                                const value = try env.eval(.{ .expression = entry.value_ptr.*.* });

                                if (!struct_literal_default_fields.contains(ident_value))
                                    return error.NotAValidStructField;

                                const field: object.Struct.Field = .{ .value = value, .name = ident_value };
                                try fields.put(ident_value, field);
                            }

                            try env.gc.put(fields, .structure);

                            return .{
                                .structure = .{
                                    .fields = fields,
                                    .type = struct_type.type.name,
                                },
                            };
                        },
                    }

                    @panic("Not Implemented");
                },

                .method_expression => |met| blk: {
                    const left = try env.eval(.{ .expression = met.caller.* });
                    const ident = object.BuiltinMethod{ .method_name = met.method };
                    break :blk try left.applyMethod(&ident);
                },
            };
        },

        .statement => |stmt| {
            return switch (stmt) {
                // .defer_statement => |defer_stmt| blk: {
                //     // try env.to_defer.append(defer_stmt.body.statements);
                //     break :blk object.NULL;
                // },
                //
                else => unreachable,

                .struct_statement => |*str| b: {
                    const struct_type: object.Object = try env.evalStructStatement(str);
                    break :b try env.setConst(str.name.value, struct_type);
                },

                .program_statement => |program| try env.evalProgram(program.statements.items),

                .block_statement => |block| try env.evalBlockStatement(block.statements),

                .expression_statement => |exp_stmt| env.eval(.{ .expression = exp_stmt.expression.* }),

                .var_block_statement => |vars| for (vars.vars_decl) |var_stmt| {
                    const val = try env.eval(.{ .expression = var_stmt.value });
                    _ = try env.setVar(var_stmt.name.value, val);
                } else object.NULL,

                .const_block_statement => |consts| for (consts.const_decl) |const_stmt| {
                    const val = try env.eval(.{ .expression = const_stmt.value });
                    _ = try env.setConst(const_stmt.name.value, val);
                } else object.NULL,

                .var_statement => |var_stmt| blk: {
                    const val = try env.eval(.{ .expression = var_stmt.value });
                    break :blk try env.setVar(var_stmt.name.value, val);
                },

                .const_statement => |const_stmt| blk: {
                    const val = try env.eval(.{ .expression = const_stmt.value });
                    break :blk try env.setConst(const_stmt.name.value, val);
                },

                .function_statement => |func_stmt| blk: {
                    const val = try env.eval(.{ .expression = func_stmt.func });
                    break :blk try env.setConst(func_stmt.name.value, val);
                },

                .return_statement => |*ret_stmt| try env.evalReturn(ret_stmt),

                .break_statement => |*break_stmt| try env.evalBreak(break_stmt),
            };
        },
    }
}

fn evalReturn(env: *Environment, ret_stmt: *const ast.ReturnStatement) !object.Object {
    const stmt = try env.eval(.{ .expression = ret_stmt.value });
    return .{ .@"return" = .{ .value = &stmt } };
}

fn evalBreak(env: *Environment, ret_stmt: *const ast.BreakStatement) !object.Object {
    const stmt = try env.eval(.{ .expression = ret_stmt.value });
    return .{ .@"break" = .{ .value = &stmt } };
}

// sometimes i do not know how to write beautiful code ...
fn evalAssignment(env: *Environment, assig: ast.AssignmentExpression) !object.Object {
    switch (assig.name.*) {
        .identifier => |ident| {
            var env_ = try env.setCheck(ident.value);
            var evaluated = try env_.eval(.{ .expression = assig.value.* });

            switch (assig.token.type) {
                .@"+=", .@"-=", .@"*=", .@"/=" => {
                    // TODO: USE THE GC LOGIC
                    const current = env_.getPtr(ident.value) orelse return error.VariableNotDeclaredERR;
                    const result = try env_.evalInfixExpression(assig.operator, current, &evaluated);
                    _ = try env_.assigVar(ident.value, result);
                },

                .@"=" => {
                    _ = try env_.assigVar(ident.value, evaluated);
                },

                .@":=" => {
                    _ = try env_.setConst(ident.value, evaluated);
                },

                else => return error.UnknowOperator,
            }

            return object.NULL;
        },

        // TODO: refactor
        // USE THE GC LOGIC
        .index_expression => |exp| {
            const index_obj = try env.eval(.{ .expression = exp.index.* });
            const evaluated = try env.eval(.{ .expression = assig.value.* });
            const var_name = exp.left.identifier.value;

            if (env.get(var_name)) |*current| {
                if (current.objType() == .array) {
                    const int_index = index_obj.integer.value;
                    const uindex = @as(usize, @intCast(int_index));
                    switch (current.array.elements[uindex]) {
                        .integer => {
                            const element = &current.array.elements[uindex];
                            switch (assig.token.type) {
                                .@"=", .@"+=", .@"-=", .@"*=", .@"/=" => _ = try env.evalInfixExpression(assig.token.literal, element, @constCast(&evaluated)),
                                else => return error.UnknowOperator,
                            }
                        },
                        .string => {
                            const element = &current.array.elements[uindex];
                            switch (assig.token.type) {
                                .@"=", .@"+=" => _ = try env.evalInfixExpression(assig.token.literal, element, @constCast(&evaluated)),
                                else => return error.UnknowOperator,
                            }
                        },
                        else => return error.NotSuportedOperation,
                    }
                }

                if (current.objType() == .hash) {
                    const key = try object.Hash.Key.init(&index_obj);
                    const pair = object.Hash.Pair{ .key = index_obj, .value = evaluated };
                    var ptr = env.getPtr(var_name).?;
                    try ptr.hash.pairs.put(key, pair);
                }
            }
            return object.NULL;
        },
        else => return error.AssignmentExpressionNotDefined,
    }
}

fn evalEnumExpression(
    env: *Environment,
    enum_obj: *const ast.EnumLiteral,
) !std.StringHashMap(object.Enum.Tag) {
    var tags = std.StringHashMap(object.Enum.Tag).init(env.allocator);
    errdefer tags.deinit();

    var enum_iterator = enum_obj.tags.iterator();
    while (enum_iterator.next()) |enu| {
        const tag_name = enu.key_ptr.*;
        const tag_value = try env.eval(.{ .expression = enu.value_ptr.*.* });
        const tag = object.Enum.Tag{ .name = tag_name, .value = &tag_value };
        try tags.put(tag_name, tag);
    }

    return tags;
}

pub const TypeFields = std.StringHashMap(object.Type.Field);
pub const StructFields = std.StringHashMap(object.Struct.Field);

fn evalStructExpression(
    env: *Environment,
    struct_obj: *const ast.StructExpression,
) !object.Object {
    var fields = try env.allocator.create(TypeFields);
    errdefer env.allocator.destroy(fields);

    fields.* = TypeFields.init(env.allocator);
    errdefer fields.deinit();

    var struct_iterator = struct_obj.fields.iterator();
    while (struct_iterator.next()) |struc| {
        const field_name = struc.key_ptr.*;
        const field_value = try env.eval(.{ .expression = struc.value_ptr.*.* });
        const field = object.Type.Field{ .name = field_name, .value = field_value };
        try fields.put(field_name, field);
    }

    if (fields.count() > 0)
        try env.gc.put(fields, .type);

    return .{
        .type = .{
            .name = struct_obj.name.value,
            .fields = fields,
        },
    };
}

fn evalStructStatement(
    env: *Environment,
    struct_obj: *const ast.StructStatement,
) !object.Object {
    var fields = try env.allocator.create(TypeFields);
    errdefer env.allocator.destroy(fields);

    fields.* = TypeFields.init(env.allocator);
    errdefer fields.deinit();

    var struct_iterator = struct_obj.fields.iterator();
    while (struct_iterator.next()) |struc| {
        const field_name = struc.key_ptr.*;
        const field_value = try env.eval(.{ .expression = struc.value_ptr.*.* });
        const field = object.Type.Field{ .name = field_name, .value = field_value };
        try fields.put(field_name, field);
    }

    if (fields.count() > 0)
        try env.gc.put(fields, .type);

    return .{
        .type = .{
            .name = struct_obj.name.value,
            .fields = fields,
        },
    };
}

pub const KeyParHash = std.AutoHashMap(object.Hash.Key, object.Hash.Pair);

fn evalHashExpression(
    env: *Environment,
    hash_obj: *const ast.HashLiteral,
) !*KeyParHash {
    var pairs = try env.allocator.create(KeyParHash);
    errdefer env.allocator.destroy(pairs);

    pairs.* = KeyParHash.init(env.allocator);
    errdefer pairs.deinit();

    var hash_iterator = hash_obj.pairs.iterator();
    while (hash_iterator.next()) |hash| {
        const key = env.eval(.{ .expression = hash.key_ptr.*.* }) catch |err| b: {
            std.debug.print("{s}", .{@errorName(err)});
            const ident = hash.key_ptr.*.identifier;
            break :b object.Object{ .builtin_method = .{ .method_name = ident } };
        };
        const val = try env.eval(.{ .expression = hash.value_ptr.*.* });

        const hash_key = try object.Hash.Key.init(&key);
        const hash_pair: object.Hash.Pair = .{ .key = key, .value = val };

        try pairs.put(hash_key, hash_pair);
    }

    try env.gc.put(pairs, .hash);

    return pairs;
}

fn evalRange(env: *Environment, range: *const ast.RangeExpression) !object.Object {
    var result = std.ArrayList(object.Object).init(env.allocator);
    errdefer result.deinit();

    const start = try env.eval(.{ .expression = range.start.* });
    if (start.objType() != .integer) return error.RangeStartIndexMustBeAnInteger;

    const end = try env.eval(.{ .expression = range.end.* });
    if (end.objType() != .integer) return error.RangeEndIndexMustBeAnInteger;

    const s: usize = @intCast(start.integer.value);
    const e: usize = @intCast(end.integer.value);

    if (s < 0 or e < 0) {
        return error.RangeMustBePositive;
    }

    if (s > e) {
        return error.InvalidIndexRange;
    }

    for (s..e) |i| {
        try result.append(.{ .integer = .{ .value = @intCast(i) } });
    }

    const slice = try result.toOwnedSlice();

    if (slice.len > 0) try env.gc.put(slice, .{ .objeto = slice.len });

    return .{ .array = .{ .elements = slice } };
}

fn evalExpressions(env: *Environment, exps: []ast.Expression) ![]object.Object {
    var result = std.ArrayList(object.Object).init(env.allocator);
    errdefer result.deinit();

    for (exps) |exp| {
        const evaluated = try env.eval(.{ .expression = exp });
        try result.append(evaluated);
    }

    const slice = try result.toOwnedSlice();

    if (slice.len > 0) try env.gc.put(slice, .{ .objeto = slice.len });

    return slice;
}

fn evalProgram(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try env.eval(.{ .statement = statement });
        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => continue,
        }
    }
    return result;
}

pub fn evalBlockStatement(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
    var result: object.Object = undefined;

    for (stmts) |statement| {
        result = try env.eval(.{ .statement = statement });
        if (result == .@"return") return result;
    }

    return result;
}

fn evalStatement(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try env.eval(.{ .statement = statement });
        if (result == .@"return") return result.@"return".value.*;
    }
    return result;
}

fn evalIfExpression(env: *Environment, ie: *const ast.IfExpression) anyerror!object.Object {
    const condition = try env.eval(.{ .expression = ie.condition.* });

    // TODO: return an error
    if (condition != .boolean) return object.NULL;

    if (condition.boolean.value) {
        // { block statements if}
        return try env.evalBlockStatement(ie.consequence.statements);
    }

    if (ie.alternative) |alternative| {
        // { block statement else }
        return try env.evalBlockStatement(alternative.statements);
    }

    return object.NULL;
}

fn evalInfixIntInt(op: []const u8, left: *object.Object, right: *object.Object) object.Object {
    return if (eql(u8, op, "+"))
        .{ .integer = .{ .value = left.integer.value + right.integer.value } }
    else if (eql(u8, op, "-"))
        .{ .integer = .{ .value = left.integer.value - right.integer.value } }
    else if (eql(u8, op, "*"))
        .{ .integer = .{ .value = left.integer.value * right.integer.value } }
    else if (eql(u8, op, "/"))
        .{ .integer = .{ .value = @divFloor(left.integer.value, right.integer.value) } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = left.integer.value > right.integer.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = left.integer.value < right.integer.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = left.integer.value == right.integer.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = left.integer.value != right.integer.value } }
    else if (eql(u8, op, "=")) b: {
        left.integer.value = right.integer.value;
        break :b left.*;
    } else if (eql(u8, op, "+=")) b: {
        left.integer.value += right.integer.value;
        break :b left.*;
    } else if (eql(u8, op, "-=")) b: {
        left.integer.value -= right.integer.value;
        break :b left.*;
    } else if (eql(u8, op, "*=")) b: {
        left.integer.value *= right.integer.value;
        break :b left.*;
    } else if (eql(u8, op, "/=")) b: {
        left.integer.value = @divFloor(left.integer.value, right.integer.value);
        break :b left.*;
    } else object.NULL;
}

fn evalInfixFloatFloat(op: []const u8, left: *object.Object, right: *object.Object) object.Object {
    return if (eql(u8, op, "+"))
        .{ .float = .{ .value = left.float.value + right.float.value } }
    else if (eql(u8, op, "-"))
        .{ .float = .{ .value = left.float.value - right.float.value } }
    else if (eql(u8, op, "*"))
        .{ .float = .{ .value = left.float.value * right.float.value } }
    else if (eql(u8, op, "/"))
        .{ .float = .{ .value = left.float.value / right.float.value } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = left.float.value > right.float.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = left.float.value < right.float.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = left.float.value == right.float.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = left.float.value != right.float.value } }
    else if (eql(u8, op, "=")) b: {
        left.float.value = right.float.value;
        break :b left.*;
    } else if (eql(u8, op, "+=")) b: {
        left.float.value += right.float.value;
        break :b left.*;
    } else if (eql(u8, op, "-=")) b: {
        left.float.value -= right.float.value;
        break :b left.*;
    } else if (eql(u8, op, "*=")) b: {
        left.float.value *= right.float.value;
        break :b left.*;
    } else if (eql(u8, op, "/=")) b: {
        left.float.value = left.float.value / right.float.value;
        break :b left.*;
    } else object.NULL;
}

fn evalInfixIntFloat(op: []const u8, left: *object.Object, right: *object.Object) object.Object {
    const leth_float: f64 = @floatFromInt(left.integer.value);
    return if (eql(u8, op, "+"))
        .{ .float = .{ .value = leth_float + right.float.value } }
    else if (eql(u8, op, "-"))
        .{ .float = .{ .value = leth_float - right.float.value } }
    else if (eql(u8, op, "*"))
        .{ .float = .{ .value = leth_float * right.float.value } }
    else if (eql(u8, op, "/"))
        .{ .float = .{ .value = leth_float / right.float.value } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = leth_float > right.float.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = leth_float < right.float.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = leth_float == right.float.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = leth_float != right.float.value } }
    else if (eql(u8, op, "=")) b: {
        left.integer.value = @intFromFloat(right.float.value);
        break :b left.*;
    } else if (eql(u8, op, "+=")) b: {
        left.integer.value += @intFromFloat(right.float.value);
        break :b left.*;
    } else if (eql(u8, op, "-=")) b: {
        left.integer.value -= @intFromFloat(right.float.value);
        break :b left.*;
    } else if (eql(u8, op, "*=")) b: {
        left.integer.value *= @intFromFloat(right.float.value);
        break :b left.*;
    } else if (eql(u8, op, "/=")) b: {
        left.integer.value = @intFromFloat(@divFloor(@as(f64, @floatFromInt(left.integer.value)), right.float.value));
        break :b left.*;
    } else object.NULL;
}

fn evalInfixFloatInt(op: []const u8, left: *object.Object, right: *object.Object) object.Object {
    const riv: f64 = @floatFromInt(right.integer.value);
    return if (eql(u8, op, "+"))
        .{ .float = .{ .value = riv + left.float.value } }
    else if (eql(u8, op, "-"))
        .{ .float = .{ .value = riv - left.float.value } }
    else if (eql(u8, op, "*"))
        .{ .float = .{ .value = riv * left.float.value } }
    else if (eql(u8, op, "/"))
        .{ .float = .{ .value = riv / left.float.value } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = riv > left.float.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = riv < left.float.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = riv == left.float.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = riv != left.float.value } }
    else if (eql(u8, op, "=")) b: {
        left.float.value = @floatFromInt(right.integer.value);
        break :b left.*;
    } else if (eql(u8, op, "+=")) b: {
        left.float.value += riv;
        break :b left.*;
    } else if (eql(u8, op, "-=")) b: {
        left.float.value -= riv;
        break :b left.*;
    } else if (eql(u8, op, "*=")) b: {
        left.float.value *= riv;
        break :b left.*;
    } else if (eql(u8, op, "/=")) b: {
        left.float.value = left.float.value / @as(f64, @floatFromInt(right.integer.value));
        break :b left.*;
    } else object.NULL;
}

fn evalInfixBool(op: []const u8, left: *object.Object, right: *object.Object) object.Object {
    return if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } }
    else if (eql(u8, op, "or"))
        .{ .boolean = .{ .value = left.boolean.value or right.boolean.value } }
    else if (eql(u8, op, "and"))
        .{ .boolean = .{ .value = left.boolean.value and right.boolean.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } }
    else
        object.NULL;
}

fn evalInfixExpression(env: *Environment, op: []const u8, left: *object.Object, right: *object.Object) !object.Object {
    // int init operation
    if (right.objType() == .integer and left.objType() == .integer) {
        return evalInfixIntInt(op, left, right);
    }

    if (right.objType() == .float and left.objType() == .float) {
        return evalInfixFloatFloat(op, left, right);
    }

    if (left.objType() == .integer and right.objType() == .float) {
        return evalInfixIntFloat(op, left, right);
    }

    if (left.objType() == .float and right.objType() == .integer) {
        return evalInfixFloatInt(op, left, right);
    }

    // bool bool operation
    if (right.objType() == .boolean and left.objType() == .boolean) {
        return evalInfixBool(op, left, right);
    }

    if (right.objType() == .null or left.objType() == .null) {
        return if (eql(u8, op, "=="))
            .{ .boolean = .{ .value = left.objType() == right.objType() } }
        else if (eql(u8, op, "!="))
            .{ .boolean = .{ .value = left.objType() != right.objType() } }
        else
            object.NULL;
    }

    // string string op
    if (right.objType() == .string and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left.string.value, right.string.value });
            if (new_string.len > 0)
                try env.gc.put(new_string, .{ .string = new_string.len });
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left.string.value, right.string.value });
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .integer and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .string and left.objType() == .integer) {
        return if (eql(u8, op, "+")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            // try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            left.* = .{ .string = .{ .value = new_string } };
            break :b left.*;
        } else object.NULL;
    }

    if (left.objType() == .array and right.objType() == .array) {
        return if (eql(u8, op, "+")) b: {
            const l_arr = left.array.elements;
            const r_arr = right.array.elements;
            var array = try std.ArrayList(object.Object).initCapacity(env.allocator, l_arr.len + r_arr.len);
            errdefer array.deinit();

            try array.appendSlice(l_arr);
            try array.appendSlice(r_arr);

            const x = try array.toOwnedSlice();

            if (x.len > 0) try env.gc.put(x, .{ .objeto = x.len });

            break :b .{ .array = .{ .elements = x } };
        } else if (eql(u8, op, "+=")) b: {
            const l_arr = left.array.elements;
            const r_arr = right.array.elements;
            var array = try std.ArrayList(object.Object).initCapacity(env.allocator, l_arr.len + r_arr.len);
            errdefer array.deinit();

            try array.appendSlice(l_arr);
            try array.appendSlice(r_arr);

            const x = try array.toOwnedSlice();

            if (x.len > 0) try env.gc.put(x, .{ .objeto = x.len });

            left.array.elements = x;

            break :b left.*;
        } else object.NULL;
    }

    return object.NULL;
}

fn evalForLoopExpression(env: *Environment, fl: *const ast.ForLoopExpression) !object.Object {
    while (true) {
        const condition = try env.eval(.{ .expression = fl.condition.* });
        if (condition != .boolean) return object.NULL;
        if (!condition.boolean.value) return object.NULL;

        var rt2 = try env.evalBlockStatement(fl.consequence.statements);
        if (rt2.objType() == .@"return") return rt2;
    }
}

/// TODO: refactor
fn evalForLoopRangeExpression(env: *Environment, fl: *const ast.ForLoopRangeExpression) !object.Object {
    const iterable = try env.eval(.{ .expression = fl.iterable.* });

    switch (iterable) {
        .array => |arr| {
            env.gc.incRef(iterable);

            if (env.store.contains(fl.ident)) return error.LoopIndexIniAmbiguous;

            for (arr.elements, 0..) |element, i| {
                var _env = env.newEncloseEnv();
                defer {
                    _env.store.deinit();
                    _env.is_const.deinit();
                }

                _ = try _env.setConst(fl.ident, element);

                if (fl.index) |index| {
                    const idx_: object.Object = .{ .integer = .{ .value = @intCast(i) } };
                    _ = try _env.setConst(index, idx_);
                }

                const evaluated_block = try _env.evalBlockStatement(fl.body.statements);

                // if (evaluated_block.objType() == .@"continue") return evaluated_block;
                if (evaluated_block.objType() == .@"return") return evaluated_block;
                if (evaluated_block.objType() == .@"break") return evaluated_block;
            }
        },
        .string => |str| {
            for (str.value, 0..) |char, i| {
                const ch: object.Object = .{ .char = .{ .value = char } };

                var _env = env.newEncloseEnv();
                defer _env.deinit();

                _ = try _env.setConst(fl.ident, ch);

                if (fl.index) |index| {
                    const idx_: object.Object = .{ .integer = .{ .value = @intCast(i) } };
                    _ = try _env.setConst(index, idx_);
                }

                const evaluated_block = try _env.evalBlockStatement(fl.body.statements);

                // if (evaluated_block.objType() == .@"continue") return evaluated_block;
                if (evaluated_block.objType() == .@"return") return evaluated_block;
                if (evaluated_block.objType() == .@"break") return evaluated_block;
            }
        },
        else => return error.FuckedIterableType,
    }

    return object.NULL;
}

fn evalSwitchExpression(env: *Environment, sw: *const ast.SwitchExpression) !object.Object {
    // value to switch on
    const value = try env.eval(.{ .expression = sw.value.* });

    for (sw.choices) |ch| {
        const exps = ch.exps orelse {
            const block_eval = try env.eval(.{ .statement = .{ .block_statement = ch.block } });
            return block_eval;
        };

        for (exps) |exp| {
            const swi_value = try env.eval(.{ .expression = exp });

            if (std.meta.eql(value, swi_value)) {
                const block_eval = try env.eval(.{ .statement = .{ .block_statement = ch.block } });
                return block_eval;
            }
        }
    }

    return object.NULL;
}
