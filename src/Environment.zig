const std = @import("std");
const ast = @import("ast.zig");
const object = @import("./object.zig");
const buildins = @import("./buildins.zig").buildins;
const c = @cImport(@cInclude("gc.h"));

const eql = std.mem.eql;

const Environment = @This();

/// outer scope (like globals for a scope)
allocator: std.mem.Allocator,
/// to be tested
arena: std.heap.ArenaAllocator = std.heap.ArenaAllocator.init(std.heap.c_allocator),
outer: ?*Environment = null,
permit: ?[]const []const u8 = null,

is_const: std.StringHashMap(bool),
store: std.StringHashMap(object.Object),
allocated_str: std.ArrayList([]u8),
/// arrays, function arguments
allocated_obj_list: std.ArrayList([]object.Object),
allocated_hash: std.ArrayList(std.AutoHashMap(object.Hash.Key, object.Hash.Pair)),

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .store = std.StringHashMap(object.Object).init(allocator),
        .is_const = std.StringHashMap(bool).init(allocator),
        .allocated_str = std.ArrayList([]u8).init(allocator),
        .allocated_obj_list = std.ArrayList([]object.Object).init(allocator),
        // .allocated_enum = std.ArrayList(std.StringHashMap(object.Enum.Tag)).init(allocator),
        .allocated_hash = std.ArrayList(std.AutoHashMap(object.Hash.Key, object.Hash.Pair)).init(allocator),
        // .to_defer = std.ArrayList([]ast.Statement).init(allocator),
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

/// this function deallocates ALL OBJECTS from a given Environment; return from this Environment sucks HAHAHAH
pub fn deinit(self: *Environment) void {
    self.arena.deinit();

    // for (self.to_defer.items) |stmtx| {
    //     _ = self.evalBlockStatement(stmtx) catch null;
    // }

    for (self.allocated_str.items) |item| {
        self.allocator.free(item);
    }

    for (self.allocated_obj_list.items) |item| {
        self.allocator.free(item);
    }

    for (self.allocated_hash.items) |*item| {
        item.deinit();
    }

    // for (self.allocated_enum.items) |*item| {
    //     item.deinit();
    // }

    // self.to_defer.deinit();
    self.store.deinit();
    self.is_const.deinit();
    self.allocated_str.deinit();
    self.allocated_obj_list.deinit();
    self.allocated_hash.deinit();
    // self.allocated_enum.deinit();
}

pub fn get(env: *Environment, name: []const u8) ?object.Object {
    return env.store.get(name) orelse if (env.outer) |outer| outer.get(name) else null;
}

pub fn _get(env: *Environment, name: []const u8) ?object.Object {
    return env.store.get(name) orelse null;
}

pub fn getFatherEnv(env: *Environment, name: []const u8) *Environment {
    if (env.outer) |outer| return outer.getFatherEnv(name);

    if (env._get(name)) |_| {
        return env;
    }

    return env;
}

pub fn getPtr(env: *Environment, name: []const u8) ?*object.Object {
    return env.store.getPtr(name) orelse if (env.outer) |outer| outer.getPtr(name) else null;
}

/// is the variable exists, it's main *env is returned
/// BUG: var i = 0 var i = 2 nao deveria ser valido
pub fn setCheck(env: *Environment, name: []const u8) !*Environment {
    // verifica se o env outer possui var com valor const
    const is_const = env.is_const.get(name) orelse false;
    if (is_const) return error.CanNotReassingConstVariables;

    if (env.outer) |outer_env| {
        const is_const_ = outer_env.is_const.get(name) orelse false;
        if (is_const_) return error.CanNotReassingConstVariables;
        return try outer_env.setCheck(name);
    }

    return env;
}

pub fn setVar(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    if (env.get(name)) |_| {
        return error.VariableCanNotBeRedeclared;
    }

    _ = try env.setCheck(name);
    try env.store.put(name, obj);
    try env.is_const.put(name, false);
    return obj;
}

pub fn assigVar(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    _ = env.get(name) orelse return error.VariableNotDeclared;
    const var_env = try env.setCheck(name);
    try var_env.store.put(name, obj);
    return obj;
}

pub fn dropScopeVar(env: *Environment, name: []const u8) void {
    _ = env.is_const.remove(name);
    _ = env.store.remove(name);
}

pub fn setConst(env: *Environment, name: []const u8, obj: object.Object) !object.Object {
    _ = try env.setCheck(name);
    try env.store.put(name, obj);
    try env.is_const.put(name, true);
    return obj;
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

    return error.IdentifierNotFound;
}

pub fn eval(env: *Environment, node: ast.Node) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp.*) {
                .null_literal => object.NULL,

                .range => |*range| try env.evalRange(range),

                // .type => object.NULL,

                .identifier => |iden| try env.evalIdentifier(&iden),

                .boolean => |b| .{ .boolean = .{ .value = b.value } },

                .string_literal => |s| .{ .string = .{ .value = s.value } },

                .integer_literal => |i| .{ .integer = .{ .value = i.value } },

                .float_literal => |f| .{ .float = .{ .value = f.value } },

                .array_literal => |*array| .{ .array = .{ .elements = try env.evalExpressions(array.elements) } },

                // .enum_tag => |et| tag: {
                //     const value = try env.evalIdentifier(&et.value);
                //     break :tag .{ .enum_tag = .{ .value = &value, .name = et.value.value } };
                // },

                .hash_literal => |*hash| .{ .hash = .{ .pairs = try env.evalHashExpression(hash) } },

                // .enum_literal => |*enu| .{ .enumerator = .{ .tags = try env.evalEnumExpression(enu) } },

                .function_literal => |func| .{ .function = .{ .parameters = func.parameters, .body = func.body, .env = env } },

                .assignment_expression => |ass| try env.evalAssignment(ass),

                // .switch_expression => |*swi| try env.evalSwitchExpression(swi),

                .if_expression => |*if_exp| try env.evalIfExpression(if_exp),

                // .forloop_expression => |*fl| try env.evalForLoopExpression(fl),
                //
                // .forloop_range_expression => |*flr| try env.evalForLoopRangeExpression(flr),

                // .multi_forloop_range_expression => @panic(""),

                .prefix_expression => |pre| blk: {
                    const right = try env.eval(.{ .expression = pre.right });
                    break :blk right.evalPrefixExpression(pre.operator);
                },

                .infix_expression => |pre| blk: {
                    var left = try env.eval(.{ .expression = pre.left });
                    var right = try env.eval(.{ .expression = pre.right });
                    break :blk try env.evalInfixExpression(pre.operator, &left, &right);
                },

                .call_expression => |call| blk: {
                    var func = try env.eval(.{ .expression = call.function });
                    const args = try env.evalExpressions(call.arguments);
                    break :blk try func.applyFunction(args);
                },

                .index_expression => |idx| blk: {
                    const left = try env.eval(.{ .expression = idx.left });
                    const index = try env.eval(.{ .expression = idx.index });
                    break :blk left.evalIndexExpression(&index);
                },

                .method_expression => |met| blk: {
                    const left = try env.eval(.{ .expression = met.caller });
                    const ident: object.BuiltinMethod = .{ .method_name = met.method };
                    break :blk try left.applyMethod(&ident);
                },

                else => @panic("Not Implemented"),
            };
        },

        .statement => |stmt| {
            return switch (stmt) {
                // .defer_statement => |defer_stmt| blk: {
                //     try env.to_defer.append(defer_stmt.body.statements);
                //     break :blk object.NULL;
                // },

                .program_statement => |program| try env.evalProgram(program.statements.items),

                .block_statement => |block| try env.evalBlockStatement(block.statements),

                .expression_statement => |exp_stmt| env.eval(.{ .expression = exp_stmt.expression }),

                // .var_block_statement => |vars| for (vars.vars_decl) |var_stmt| {
                //     const val = try env.eval(.{ .expression = var_stmt.value });
                //     _ = try env.setVar(var_stmt.name.value, val);
                // } else object.NULL,
                //
                // .const_block_statement => |consts| for (consts.const_decl) |const_stmt| {
                //     const val = try env.eval(.{ .expression = const_stmt.value });
                //     _ = try env.setConst(const_stmt.name.value, val);
                // } else object.NULL,

                .var_statement => |var_stmt| blk: {
                    const val = try env.eval(.{ .expression = var_stmt.value });
                    break :blk try env.setVar(var_stmt.name.value, val);
                },

                .const_statement => |const_stmt| blk: {
                    const val = try env.eval(.{ .expression = const_stmt.value });
                    break :blk try env.setConst(const_stmt.name.value, val);
                },

                // .function_statement => |func_stmt| blk: {
                //     const val = try env.eval(.{ .expression = func_stmt.func });
                //     break :blk try env.setConst(func_stmt.name.value, val);
                // },

                .return_statement => |*ret_stmt| blk: {
                    const obj = try env.evalReturn(ret_stmt);
                    break :blk obj;
                },

                .break_statement => |*break_stmt| try env.evalBreak(break_stmt),

                else => @panic("Statement not implemented"),
            };
        },
    }
}

fn evalReturn(env: *Environment, ret_stmt: *const ast.ReturnStatement) !object.Object {
    const stmt_ = try env.eval(.{ .expression = ret_stmt.value });

    // if (env.outer) |outer| {
    //     const obj = try outer.allocator.create(object.Object);
    //     obj.* = value;
    //     return .{ .@"return" = .{ .value = obj } };
    // }

    // this approach sucks !!! I KNOW
    const value: object.Object = switch (stmt_) {
        else => stmt_,

        .string => |str| blk: {
            const copy_str = try env.outer.?.allocator.alloc(u8, str.value.len);
            @memcpy(copy_str, @constCast(str.value));
            try env.outer.?.allocated_str.append(copy_str);
            break :blk .{ .string = .{ .value = copy_str } };
        },

        .array => |arr| blk: {
            const copy_elemens = try env.outer.?.allocator.alloc(object.Object, arr.elements.len);
            @memcpy(copy_elemens, arr.elements);
            try env.outer.?.allocated_obj_list.append(copy_elemens);
            break :blk .{ .array = .{ .elements = copy_elemens } };
        },
    };

    return .{ .@"return" = .{ .value = &value } };
}

fn evalBreak(env: *Environment, ret_stmt: *const ast.BreakStatement) !object.Object {
    const stmt_ = try env.eval(.{ .expression = ret_stmt.value });

    // this approach sucks !!! I KNOW
    const value: object.Object = switch (stmt_) {
        else => stmt_,

        .string => |str| blk: {
            const copy_str = try env.outer.?.allocator.alloc(u8, str.value.len);
            @memcpy(copy_str, @constCast(str.value));
            try env.outer.?.allocated_str.append(copy_str);
            break :blk .{ .string = .{ .value = copy_str } };
        },

        .array => |arr| blk: {
            const copy_elemens = try env.outer.?.allocator.alloc(object.Object, arr.elements.len);
            @memcpy(copy_elemens, arr.elements);
            try env.outer.?.allocated_obj_list.append(copy_elemens);
            break :blk .{ .array = .{ .elements = copy_elemens } };
        },
    };

    return .{ .@"break" = .{ .value = &value } };
}

// sometimes i do not know how to write beautiful code ...
fn evalAssignment(env: *Environment, assig: ast.AssignmentExpression) !object.Object {
    switch (assig.name.*) {
        .identifier => |ident| {
            var env_ = try env.setCheck(ident.value);
            var evaluated = try env_.eval(.{ .expression = assig.value });

            switch (assig.token.type) {
                .@"+=", .@"-=", .@"*=", .@"/=" => {
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

        .index_expression => |exp| {
            const index_obj = try env.eval(.{ .expression = exp.index });
            const evaluated = try env.eval(.{ .expression = assig.value });
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

                // if (current.objType() == .hash) {
                //     const key = try object.Hash.Key.init(&index_obj);
                //     const pair = object.Hash.Pair{ .key = index_obj, .value = evaluated };
                //     var ptr = env.getPtr(var_name).?;
                //     try ptr.hash.pairs.put(key, pair);
                // }
            }
            return object.NULL;
        },
        else => return error.AssignmentExpressionNotDefined,
    }
}

// fn evalEnumExpression(
//     env: *Environment,
//     enum_obj: *const ast.EnumLiteral,
// ) !std.StringHashMap(object.Enum.Tag) {
//     var tags = std.StringHashMap(object.Enum.Tag).init(env.allocator);
//     errdefer tags.deinit();
//
//     var enum_iterator = enum_obj.tags.iterator();
//     while (enum_iterator.next()) |enu| {
//         const tag_name = enu.key_ptr.*;
//         const tag_value = try env.eval(.{ .expression = enu.value_ptr.*.* });
//         const tag = object.Enum.Tag{ .name = tag_name, .value = &tag_value };
//         try tags.put(tag_name, tag);
//     }
//
//     try env.allocated_enum.append(tags);
//
//     return tags;
// }

fn evalHashExpression(
    env: *Environment,
    hash_obj: *const ast.HashLiteral,
) !std.AutoHashMap(object.Hash.Key, object.Hash.Pair) {
    var pairs = std.AutoHashMap(object.Hash.Key, object.Hash.Pair).init(env.allocator);
    errdefer pairs.deinit();

    var hash_iterator = hash_obj.pairs.iterator();
    while (hash_iterator.next()) |*hash| {
        var key_value = try env.allocator.alloc(object.Object, 2);
        errdefer env.allocator.free(key_value);

        // key_value[0] = env.eval(.{ .expression = hash.key_ptr.* }) catch |err| switch (err) {
        //     // teste
        //     // TODO: m.x onde x é um hash_tag, e não um identifier (ou algo assim)
        //     error.IdentifierNotFound => .{ .builtin_method = .{ .method_name = hash.key_ptr.*.*.identifier } },
        //     else => return err,
        // };
        key_value[0] = try env.eval(.{ .expression = hash.key_ptr.* });
        key_value[1] = try env.eval(.{ .expression = hash.value_ptr.* });

        var key = key_value[0];
        const val = key_value[1];

        const hash_key: object.Hash.Key = try .init(&key);
        const hash_pair: object.Hash.Pair = .{ .key = key, .value = val };

        try pairs.put(hash_key, hash_pair);
        try env.allocated_obj_list.append(key_value);
    }

    try env.allocated_hash.append(pairs);

    return pairs;
}

fn evalRange(env: *Environment, range: *const ast.RangeExpression) !object.Object {
    var result = std.ArrayList(object.Object).init(env.allocator);
    errdefer result.deinit();

    const start = try env.eval(.{ .expression = range.start });
    if (start.objType() != .integer) return error.RangeStartIndexMustBeAnInteger;

    const end = try env.eval(.{ .expression = range.end });
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

    const result_owned = try result.toOwnedSlice();

    try env.allocated_obj_list.append(result_owned);

    return .{ .array = .{ .elements = result_owned } };
}

fn evalExpressions(env: *Environment, exps: []*ast.Expression) ![]object.Object {
    // TODO 144
    var result = std.ArrayList(object.Object).init(env.allocator);
    errdefer result.deinit();

    for (exps) |e| {
        const evaluated = try env.eval(.{ .expression = e });
        try result.append(evaluated);
    }

    const result_owned = try result.toOwnedSlice();
    try env.allocated_obj_list.append(result_owned);

    return result_owned;
}

fn evalProgram(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try env.eval(.{ .statement = statement });
        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => {},
        }
    }
    return result;
}

fn evalBlockStatement(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
    var result: object.Object = undefined;

    var new_env = env.newEncloseEnv();
    defer new_env.deinit();

    for (stmts) |statement| {
        result = try new_env.eval(.{ .statement = statement });
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
    const condition = try env.eval(.{ .expression = ie.condition });

    if (condition != .boolean) return object.NULL;

    if (condition.boolean.value) {
        // { block statements if}
        return try env.eval(.{ .statement = .{ .block_statement = ie.consequence } });
    }

    if (ie.alternative) |alternative| {
        // { block statement else }
        return try env.eval(.{ .statement = .{ .block_statement = alternative } });
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
    return if (eql(u8, op, "+"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(left.integer.value)) + right.float.value } }
    else if (eql(u8, op, "-"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(left.integer.value)) - right.float.value } }
    else if (eql(u8, op, "*"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(left.integer.value)) * right.float.value } }
    else if (eql(u8, op, "/"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(left.integer.value)) / right.float.value } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(left.integer.value)) > right.float.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(left.integer.value)) < right.float.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(left.integer.value)) == right.float.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(left.integer.value)) != right.float.value } }
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
    return if (eql(u8, op, "+"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(right.integer.value)) + left.float.value } }
    else if (eql(u8, op, "-"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(right.integer.value)) - left.float.value } }
    else if (eql(u8, op, "*"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(right.integer.value)) * left.float.value } }
    else if (eql(u8, op, "/"))
        .{ .float = .{ .value = @as(f64, @floatFromInt(right.integer.value)) / left.float.value } }
    else if (eql(u8, op, ">"))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(right.integer.value)) > left.float.value } }
    else if (eql(u8, op, "<"))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(right.integer.value)) < left.float.value } }
    else if (eql(u8, op, "=="))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(right.integer.value)) == left.float.value } }
    else if (eql(u8, op, "!="))
        .{ .boolean = .{ .value = @as(f64, @floatFromInt(right.integer.value)) != left.float.value } }
    else if (eql(u8, op, "=")) b: {
        left.float.value = @floatFromInt(right.integer.value);
        break :b left.*;
    } else if (eql(u8, op, "+=")) b: {
        left.float.value += @floatFromInt(right.integer.value);
        break :b left.*;
    } else if (eql(u8, op, "-=")) b: {
        left.float.value -= @floatFromInt(right.integer.value);
        break :b left.*;
    } else if (eql(u8, op, "*=")) b: {
        left.float.value *= @floatFromInt(right.integer.value);
        break :b left.*;
    } else if (eql(u8, op, "/=")) b: {
        left.float.value = left.float.value / @as(f64, @floatFromInt(right.integer.value));
        break :b left.*;
    } else object.NULL;
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
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left.string.value, right.string.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .integer and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .string and left.objType() == .integer) {
        return if (eql(u8, op, "+")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            const new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
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
            try env.allocated_obj_list.append(x);

            break :b .{ .array = .{ .elements = x } };
        } else if (eql(u8, op, "+=")) b: {
            const l_arr = left.array.elements;
            const r_arr = right.array.elements;
            var array = try std.ArrayList(object.Object).initCapacity(env.allocator, l_arr.len + r_arr.len);
            errdefer array.deinit();

            try array.appendSlice(l_arr);
            try array.appendSlice(r_arr);

            const x = try array.toOwnedSlice();
            try env.allocated_obj_list.append(x);

            left.array.elements = x;

            break :b left.*;
        } else object.NULL;
    }

    return object.NULL;
}

fn evalForLoopExpression(env: *Environment, fl: *const ast.ForLoopExpression) !object.Object {
    while (true) {
        const condition = try env.eval(.{ .expression = fl.condition });
        if (condition != .boolean) return object.NULL;
        if (!condition.boolean.value) return object.NULL;

        var rt2 = try env.evalBlockStatement(fl.consequence.statements);
        if (rt2.objType() == .@"return") return rt2;
    }
}

fn evalForLoopRangeExpression(env: *Environment, fl: *const ast.ForLoopRangeExpression) !object.Object {
    var iterabol = try env.eval(.{ .expression = fl.iterable });

    var permit0 = std.ArrayList([]const u8).init(env.allocator);
    defer permit0.deinit();

    try permit0.append(fl.ident);

    if (fl.index) |index| {
        try permit0.append(index);
    }

    const permit = try permit0.toOwnedSlice();
    defer env.allocator.free(permit);

    var info = try iterabol.next();
    var element = info.element;
    var idx = info.index;
    var ok = info.ok;

    while (ok) {
        var new_env = env.newEncloseEnv();

        if (!(fl.ident.len == 1 and fl.ident[0] == '_')) {
            if (env.get(fl.ident)) |_| {
                return error.LoopIndexInAmbiguous;
            }
            _ = try new_env.setConst(fl.ident, element);
        }

        if (fl.index) |index| if (!(index.len == 1 and index[0] == '_')) {
            if (env.get(index)) |_| {
                return error.LoopIndexInAmbiguous;
            }
            _ = try new_env.setConst(index, idx);
        };

        // var block_eval = try env.eval(.{ .statement = .{ .block_statement = fl.body } });
        var block_eval = try new_env.evalBlockStatement(fl.body.statements);

        // drop the index and val
        // new_env.dropScopeVar(fl.ident);
        // if (fl.index) |index| new_env.dropScopeVar(index);
        new_env.deinit();

        if (block_eval.objType() == .@"return") return block_eval;

        info = try iterabol.next();
        ok = info.ok;
        idx = info.index;
        element = info.element;
    }

    return object.NULL;
}

// fn evalSwitchExpression(env: *Environment, sw: *const ast.SwitchExpression) !object.Object {
//     // value to switch on
//     const value = try env.eval(.{ .expression = sw.value });
//
//     for (sw.choices) |ch| {
//         const exps = ch.exps orelse {
//             const block_eval = try env.eval(.{ .statement = .{ .block_statement = ch.block } });
//             return block_eval;
//         };
//
//         for (exps) |exp| {
//             const swi_value = try env.eval(.{ .expression = exp });
//
//             if (std.meta.eql(value, swi_value)) {
//                 const block_eval = try env.eval(.{ .statement = .{ .block_statement = ch.block } });
//                 return block_eval;
//             }
//         }
//     }
//
//     return object.NULL;
// }
