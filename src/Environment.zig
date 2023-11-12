const std = @import("std");
const ast = @import("ast.zig");
const object = @import("./object.zig");
const eql = std.mem.eql;

const Environment = @This();

/// outer scope (like globals for a scope)
/// TODO: arena ?
allocator: std.mem.Allocator,
outer: ?*Environment = null,
permit: ?[]const []const u8 = null,

is_const: std.StringHashMap(bool),
allocated_str: std.ArrayList([]u8),
store: std.StringHashMap(object.Object),
allocated_obj: std.ArrayList([]object.Object),
allocated_hash: std.ArrayList(std.AutoHashMap(*object.Object, *object.Object)),

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .store = std.StringHashMap(object.Object).init(allocator),
        .is_const = std.StringHashMap(bool).init(allocator),
        .allocated_obj = std.ArrayList([]object.Object).init(allocator),
        .allocated_str = std.ArrayList([]u8).init(allocator),
        .allocated_hash = std.ArrayList(std.AutoHashMap(*object.Object, *object.Object)).init(allocator),
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
    for (self.allocated_str.items) |item| {
        self.allocator.free(item);
    }

    for (self.allocated_obj.items) |item| {
        self.allocator.free(item);
    }

    for (self.allocated_hash.items) |*item| {
        item.deinit();
    }

    self.store.deinit();
    self.is_const.deinit();
    self.allocated_str.deinit();
    self.allocated_obj.deinit();
    self.allocated_hash.deinit();
}

pub fn get(self: *Environment, name: []const u8) ?object.Object {
    return self.store.get(name);
}

pub fn set(self: *Environment, name: []const u8, obj: object.Object) !object.Object {
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

pub fn setConst(self: *Environment, name: []const u8, obj: object.Object) !object.Object {
    if (self.is_const.get(name)) |_| return error.CanNotReassingConstVariables;
    try self.store.put(name, obj);
    try self.is_const.put(name, true);
    return obj;
}

fn pprint(arg: *object.Object) void {
    switch (arg.*) {
        .null => |n| std.debug.print("{any}", .{n.value}),
        .float => |n| std.debug.print("{d}", .{n.value}),
        .string => |n| std.debug.print("{s}", .{n.value}),
        .integer => |n| std.debug.print("{}", .{n.value}),
        .boolean => |n| std.debug.print("{}", .{n.value}),
        else => std.debug.print("not yet printable\n", .{}),
    }
}

fn printBuiltin(args: []const object.Object) object.Object {
    for (args) |arg| {
        switch (arg) {
            .null => |n| std.debug.print("{any} ", .{n.value}),
            .float => |n| std.debug.print("{d} ", .{n.value}),
            .string => |n| std.debug.print("{s} ", .{n.value}),
            .integer => |n| std.debug.print("{} ", .{n.value}),
            .boolean => |n| std.debug.print("{} ", .{n.value}),
            .array => |n| {
                std.debug.print("{{ ", .{});

                for (n.elements, 0..) |*el, i| {
                    pprint(el);
                    if (i < n.elements.len - 1) std.debug.print(", ", .{});
                }

                std.debug.print(" }}\n", .{});
            },
            .hash => |h| {
                var iter = h.elements.iterator();
                std.debug.print("{{ ", .{});
                while (iter.next()) |entry| {
                    pprint(entry.key_ptr.*);
                    std.debug.print(":", .{});
                    pprint(entry.value_ptr.*);
                    std.debug.print(", ", .{});
                }
                std.debug.print("}}\n", .{});
            },
            else => std.debug.print("not yet printable\n", .{}),
        }
    }
    if (args.len > 1) std.debug.print("\n", .{});

    return object.NULL;
}

const buildins = std.ComptimeStringMap(object.Builtin, .{
    .{ "print", .{ .func = printBuiltin } },
});

fn evalIdentifier(env: *Environment, node: *const ast.Identifier) !object.Object {
    if (env.get(node.value)) |val| {
        return val;
    }

    if (buildins.get(node.value)) |val| {
        return .{ .builtin = val };
    }

    std.log.warn("the identifier: {s}\n", .{node.value});
    return error.IdentifierNotFound;
}

pub fn eval(env: *Environment, node: ast.Node) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .void => object.NULL,

                .identifier => |iden| try env.evalIdentifier(&iden),

                .boolean => |b| .{ .boolean = .{ .value = b.value } },

                .string_literal => |i| .{ .string = .{ .value = i.value } },

                .integer_literal => |i| .{ .integer = .{ .value = i.value } },

                .float_literal => |i| .{ .float = .{ .value = i.value } },

                .array_literal => |array| .{ .array = .{ .elements = try env.evalExpression(array.elements) } },

                .hash_literal => |*hash| .{ .hash = .{ .elements = try env.evalHashExpression(&hash.elements) } },

                .function_literal => |func| .{ .function = .{ .parameters = func.parameters, .body = func.body, .env = env } },

                .assignment_expression => |ass| try env.evalAssignment(ass),

                .switch_expression => |*swi| try env.evalSwitchExpression(swi),

                .prefix_expression => |pre| blk: {
                    const right = try env.eval(.{ .expression = pre.right.* });
                    break :blk right.evalPrefixExpression(pre.operator);
                },

                .infix_expression => |pre| blk: {
                    var left = try env.eval(.{ .expression = pre.left.* });
                    var right = try env.eval(.{ .expression = pre.right.* });
                    break :blk try env.evalInfixExpression(pre.operator, &left, &right);
                },

                .if_expression => |*if_exp| try env.evalIfExpression(if_exp),

                .call_expression => |call| blk: {
                    var func = try env.eval(.{ .expression = call.function.* });
                    const args = try env.evalExpression(call.arguments);
                    break :blk func.applyFunction(args);
                },

                .index_expression => |idx| blk: {
                    var left = try env.eval(.{ .expression = idx.left.* });
                    const index = try env.eval(.{ .expression = idx.index.* });
                    break :blk left.evalIndexExpression(&index);
                },

                .forloop_expression => |*fl| try env.evalForLoopExpression(fl),

                .forloop_range_expression => |*fl| try env.evalForLoopRangeExpression(fl),

                .method_expression => |met| b: {
                    const left = try env.eval(.{ .expression = met.caller.* });
                    const ident = object.BuiltinMethod{ .method_name = met.method };
                    break :b try left.applyMethod(&ident);
                },
            };
        },

        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| try env.evalProgram(program.statements.items),

                .block_statement => |block| try env.evalBlockStatement(block.statements),

                .expression_statement => |exp_stmt| env.eval(.{ .expression = exp_stmt.expression.* }),

                .var_block_statement => |vars| for (vars.vars_decl) |var_stmt| {
                    const val = try env.eval(.{ .expression = var_stmt.value });
                    _ = try env.set(var_stmt.name.value, val);
                } else object.NULL,

                .const_block_statement => |consts| for (consts.const_decl) |const_stmt| {
                    const val = try env.eval(.{ .expression = const_stmt.value });
                    _ = try env.setConst(const_stmt.name.value, val);
                } else object.NULL,

                .var_statement => |var_stmt| blk: {
                    const val = try env.eval(.{ .expression = var_stmt.value });
                    break :blk try env.set(var_stmt.name.value, val);
                },

                .const_statement => |const_stmt| blk: {
                    const val = try env.eval(.{ .expression = const_stmt.value });
                    break :blk try env.setConst(const_stmt.name.value, val);
                },

                .return_statement => |ret_stmt| ret_blk: {
                    var stmt_ = try env.eval(.{ .expression = ret_stmt.value });

                    // this approach sucks !!! I KNOW
                    const value: object.Object = switch (stmt_) {
                        else => stmt_,

                        .string => |str| blk: {
                            var copy_str = try env.outer.?.allocator.alloc(u8, str.value.len);
                            @memcpy(copy_str, @constCast(str.value));
                            try env.outer.?.allocated_str.append(copy_str);
                            break :blk .{ .string = .{ .value = copy_str } };
                        },

                        .array => |arr| blk: {
                            var copy_elemens = try env.outer.?.allocator.alloc(object.Object, arr.elements.len);
                            @memcpy(copy_elemens, arr.elements);
                            try env.outer.?.allocated_obj.append(copy_elemens);
                            break :blk .{ .array = .{ .elements = copy_elemens } };
                        },
                    };

                    break :ret_blk .{ .@"return" = .{ .value = &value } };
                },
            };
        },
    }
}

// sometimes i do not know how to write beautiful code ...
fn evalAssignment(env: *Environment, assig: ast.AssignmentExpression) !object.Object {
    switch (assig.name.*) {
        .identifier => |ident| {
            var evaluated = try env.eval(.{ .expression = assig.value.* });

            switch (assig.token.type) {
                .@"+=", .@"-=", .@"*=", .@"/=" => {
                    if (env.get(ident.value)) |*current| {
                        var result = try env.evalInfixExpression(assig.operator, @constCast(current), &evaluated);
                        _ = try env.set(ident.value, result);
                        return object.NULL;
                    }
                    return error.VariableNotDeclared;
                },

                .@"=" => {
                    if (env.get(ident.value)) |_| {
                        _ = try env.set(ident.value, evaluated);
                        return object.NULL;
                    }
                    return error.VariableNotDeclared;
                },

                else => return error.UnknowOperator,
            }
        },

        .index_expression => |exp| {
            const index_obj = try env.eval(.{ .expression = exp.index.* });
            const index = index_obj.integer.value;
            const evaluated = try env.eval(.{ .expression = assig.value.* });
            const var_name = exp.left.identifier.value;

            if (env.get(var_name)) |*current| {
                const uindex = @as(usize, @intCast(index));
                switch (current.array.elements[uindex]) {
                    .integer => {
                        var element = &current.array.elements[uindex];
                        switch (assig.token.type) {
                            .@"=", .@"+=", .@"-=", .@"*=", .@"/=" => _ = try env.evalInfixExpression(assig.token.literal, element, @constCast(&evaluated)),
                            else => return error.UnknowOperator,
                        }
                    },
                    .string => {
                        var element = &current.array.elements[uindex];
                        switch (assig.token.type) {
                            .@"=", .@"+=" => _ = try env.evalInfixExpression(assig.token.literal, element, @constCast(&evaluated)),
                            else => return error.UnknowOperator,
                        }
                    },
                    else => return error.NotSuportedOperation,
                }
            }
            return object.NULL;
        },
        else => return error.AssignmentExpressionNotDefined,
    }
}

fn evalHashExpression(
    env: *Environment,
    hash_exp: *const std.AutoHashMap(*ast.Expression, *ast.Expression),
) !std.AutoHashMap(*object.Object, *object.Object) {
    var hash_obj = std.AutoHashMap(*object.Object, *object.Object).init(env.allocator);
    errdefer hash_obj.deinit();

    var iter0 = hash_exp.iterator();

    while (iter0.next()) |hash| {
        var keyval = try env.allocator.alloc(object.Object, 2);
        keyval[0] = try env.eval(.{ .expression = hash.key_ptr.*.* });
        keyval[1] = try env.eval(.{ .expression = hash.value_ptr.*.* });

        try env.allocated_obj.append(keyval);
        try hash_obj.put(&keyval[0], &keyval[1]);
    }

    try env.allocated_hash.append(hash_obj);

    return hash_obj;
}

fn evalExpression(env: *Environment, exps: []ast.Expression) ![]object.Object {
    // TODO 144
    var result = std.ArrayList(object.Object).init(env.allocator);
    errdefer result.deinit();

    for (exps) |e| {
        var evaluated = try env.eval(.{ .expression = e });
        try result.append(evaluated);
    }

    var result_owned = try result.toOwnedSlice();
    try env.allocated_obj.append(result_owned);

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
    for (stmts) |statement| {
        result = try env.eval(.{ .statement = statement });
        switch (result) {
            .@"return" => return result,
            else => {},
        }
    }
    return result;
}

fn evalStatement(env: *Environment, stmts: []ast.Statement) anyerror!object.Object {
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

fn evalIfExpression(env: *Environment, ie: *const ast.IfExpression) anyerror!object.Object {
    const condition = try env.eval(.{ .expression = ie.condition.* });

    if (condition != .boolean) return object.NULL;

    if (condition.boolean.value) {
        // { block statements if}
        return try env.eval(.{ .statement = .{ .block_statement = ie.consequence } });
    } else if (ie.alternative) |alternative| {
        // { block statement else }
        return try env.eval(.{ .statement = .{ .block_statement = alternative } });
    }

    return object.NULL;
}

fn evalInfixExpression(env: *Environment, op: []const u8, left: *object.Object, right: *object.Object) !object.Object {
    // int init operation
    if (right.objType() == .integer and left.objType() == .integer) {
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

    if (right.objType() == .float and left.objType() == .float) {
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

    if (right.objType() == .float and left.objType() == .integer) {
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

    if (right.objType() == .integer and left.objType() == .float) {
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

    // bool bool operation
    if (right.objType() == .boolean and left.objType() == .boolean) {
        return if (eql(u8, op, "=="))
            .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } }
        else if (eql(u8, op, "!="))
            .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } }
        else
            object.NULL;
    }

    // string string op
    if (right.objType() == .string and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left.string.value, right.string.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{s}{s}", .{ left.string.value, right.string.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .integer and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else object.NULL;
    }

    if (right.objType() == .string and left.objType() == .integer) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(env.allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
            left.* = .{ .string = .{ .value = new_string } };
            break :b left.*;
        } else object.NULL;
    }

    return object.NULL;
}

fn evalForLoopExpression(env: *Environment, fl: *const ast.ForLoopExpression) !object.Object {
    var rt = object.Boolean{ .value = true };

    while (true) {
        var condition = try env.eval(.{ .expression = fl.condition.* });

        if (condition != .boolean) condition = .{ .boolean = .{ .value = false } };

        if (condition.boolean.value) {
            var rt2 = try env.eval(
                .{ .statement = .{ .block_statement = fl.consequence } },
            );

            if (rt2.objType() == .@"return") return rt2;
        } else break;
    }

    return .{ .boolean = rt };
}

fn evalForLoopRangeExpression(env: *Environment, fl: *const ast.ForLoopRangeExpression) !object.Object {
    var iterabol = try env.eval(.{ .expression = fl.iterable.* });

    var permit0 = std.ArrayList([]const u8).init(env.allocator);
    defer permit0.deinit();

    try permit0.append(fl.ident);
    if (fl.index) |index|
        try permit0.append(index);

    const permit = try permit0.toOwnedSlice();
    defer env.allocator.free(permit);

    var info = try iterabol.next();
    var element = info.element;
    var idx = info.index;
    var ok = info.ok;

    while (ok) {
        _ = try env.set(fl.ident, element);

        if (fl.index) |index|
            _ = try env.set(index, idx);

        var block_eval = try env.eval(.{ .statement = .{ .block_statement = fl.body } });

        // drop the index and val
        env.dropScopeVar(fl.ident);
        if (fl.index) |index| env.dropScopeVar(index);

        if (block_eval.objType() == .@"return") return block_eval;

        info = try iterabol.next();
        ok = info.ok;
        idx = info.index;
        element = info.element;
    }

    return object.NULL;
}

fn evalSwitchExpression(env: *Environment, sw: *const ast.SwitchExpression) !object.Object {
    // value to switch on
    var value = try env.eval(.{ .expression = sw.value.* });

    for (sw.choices) |ch| {

        // if (ch.exp.* == .identifier and ch.exp.identifier.token.type == .@"else") {
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
