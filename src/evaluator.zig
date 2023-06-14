// TODO: implement errors for parsing and evaluation
const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;
const eql = std.mem.eql;

const NULL = object.Object{ .null = object.Null{} };

fn pprint(arg: *object.Object) void {
    switch (arg.*) {
        .null => |n| std.debug.print("{any}", .{n.value}),
        .integer => |n| std.debug.print("{}", .{n.value}),
        .string => |n| std.debug.print("{s}", .{n.value}),
        .boolean => |n| std.debug.print("{}", .{n.value}),
        else => std.debug.print("not yet printable\n", .{}),
    }
}

fn printBuiltin(args: []const object.Object) object.Object {
    for (args) |arg| {
        switch (arg) {
            .null => |n| std.debug.print("{any}\n", .{n.value}),
            .integer => |n| std.debug.print("{}\n", .{n.value}),
            .string => |n| std.debug.print("{s}\n", .{n.value}),
            .boolean => |n| std.debug.print("{}\n", .{n.value}),
            .array => |n| {
                std.debug.print("{{ ", .{});

                for (n.elements, 0..) |*el, i| {
                    pprint(el);
                    if (i < n.elements.len - 1) std.debug.print(", ", .{});
                }

                std.debug.print(" }}\n", .{});
            },
            else => std.debug.print("not yet printable\n", .{}),
        }
    }

    return NULL;
}

const buildins = std.ComptimeStringMap(object.Builtin, .{
    .{ "print", .{ .func = printBuiltin } },
});

fn evalIdentifier(node: *const ast.Identifier, env: *object.Environment) !object.Object {
    return if (env.get(node.value)) |val|
        val
    else if (buildins.get(node.value)) |val|
        .{ .builtin = val }
    else
        error.IdentifierNotFound;
}

pub fn eval(allocator: std.mem.Allocator, node: ast.Node, env: *object.Environment) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .void => NULL,
                .boolean => |b| .{ .boolean = .{ .value = b.value } },
                .integer_literal => |i| .{ .integer = .{ .value = i.value } },
                .string_literal => |i| .{ .string = .{ .value = i.value } },

                .array_literal => |array| bkl: {
                    var elements = try evalExpression(allocator, array.elements, env);
                    break :bkl .{ .array = .{ .elements = elements } };
                },

                .hash_literal => |*hash| bkl: {
                    var elements = try evalHashExpression(allocator, &hash.elements, env);

                    break :bkl .{ .hash = .{ .elements = elements } };
                },

                .prefix_expression => |pre| blk: {
                    const right = try eval(allocator, .{ .expression = pre.right.* }, env);
                    break :blk evalPrefixExpression(pre.operator, right);
                },
                .infix_expression => |pre| blk: {
                    var left = try eval(allocator, .{ .expression = pre.left.* }, env);
                    var right = try eval(allocator, .{ .expression = pre.right.* }, env);
                    break :blk try evalInfixExpression(allocator, pre.operator, &left, &right, env);
                },
                .if_expression => |*if_exp| try evalIfExpression(allocator, if_exp, env),

                .identifier => |iden| try evalIdentifier(&iden, env), //env.get(iden.value).?,

                .assignment_expression => |ass| try evalAssignment(allocator, ass, env),

                .function_literal => |func| return .{
                    .function = .{
                        .parameters = func.parameters,
                        .body = func.body,
                        .env = env,
                    },
                },

                .call_expression => |call| blk: {
                    var func = try eval(allocator, .{ .expression = call.function.* }, env);
                    var args = try evalExpression(allocator, call.arguments, env);
                    break :blk applyFunction(allocator, func, args);
                },

                .index_expression => |idx| blk: {
                    var left = try eval(allocator, .{ .expression = idx.left.* }, env);
                    var index = try eval(allocator, .{ .expression = idx.index.* }, env);
                    break :blk evalIndexExpression(&left, &index);
                },

                .forloop_expression => |*fl| try evalForLoopExpression(allocator, fl, env),

                .forloop_range_expression => |*fl| try evalForLoopRangeExpression(allocator, fl, env),

                .method_expression => |met| b: {
                    const left = try eval(allocator, .{ .expression = met.caller.* }, env);
                    const ident = object.BuiltinMethod{ .method_name = met.method };
                    break :b try applyMethod(&left, &ident);
                },
            };
        },

        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| try evalProgram(allocator, program.statements.items, env),

                .block_statement => |block| try evalBlockStatement(allocator, block.statements, env),

                .expression_statement => |exp_stmt| eval(allocator, .{ .expression = exp_stmt.expression.* }, env),

                .var_block_statement => |vars| for (vars.vars_decl) |var_stmt| {
                    var val = try eval(allocator, .{ .expression = var_stmt.value.? }, env);
                    _ = try env.set(var_stmt.name.value, val);
                } else NULL,

                .const_block_statement => |consts| for (consts.const_decl) |const_stmt| {
                    var val = try eval(allocator, .{ .expression = const_stmt.value.? }, env);
                    _ = try env.setConst(const_stmt.name.value, val);
                } else NULL,

                .var_statement => |var_stmt| blk: {
                    var val = try eval(allocator, .{ .expression = var_stmt.value.? }, env);
                    break :blk try env.set(var_stmt.name.value, val);
                },

                .const_statement => |const_stmt| blk: {
                    var val = try eval(allocator, .{ .expression = const_stmt.value.? }, env);
                    break :blk try env.setConst(const_stmt.name.value, val);
                },

                .return_statement => |ret_stmt| blk: {
                    const exp = if (ret_stmt.value) |exp| exp else return NULL;
                    break :blk .{
                        .@"return" = .{ .value = &(try eval(allocator, .{ .expression = exp }, env)) },
                    };
                },
            };
        },
    }
}

fn applyMethod(obj: *const object.Object, arg: *const object.BuiltinMethod) !object.Object {

    // only string and lists (for now)
    if (std.mem.eql(u8, arg.method_name.value, "len")) {
        const len = switch (obj.objType()) {
            .string => obj.string.value.len,
            .array => obj.array.elements.len,
            .hash => obj.hash.elements.count(),
            else => return error.MethodLenNotDefined,
        };
        return .{ .integer = .{ .value = @intCast(i64, len) } };
    }

    return error.MethodNotDefined;
}

fn applyFunction(allocator: std.mem.Allocator, func: object.Object, args: []object.Object) !object.Object {
    // TODO 147
    return switch (func) {
        .function => |f| b: {
            var extended_env = try extendFunctionEnv(@constCast(&f), args);
            defer extended_env.deinit();
            var evaluated = try eval(allocator, .{ .statement = .{ .block_statement = f.body } }, &extended_env);
            break :b unwrapReturnValue(evaluated);
        },

        .builtin => |b| b.func(args),

        else => error.NotAFunction,
    };
}

fn extendFunctionEnv(func: *object.Function, args: []object.Object) anyerror!object.Environment {
    // TODO: sipa tem que alocar env
    var enclose_env = object.Environment.newEncloseEnv(func.env);

    for (func.parameters, args) |param, arg|
        _ = try enclose_env.set(param.value, arg);

    return enclose_env;
}

fn unwrapReturnValue(obj: object.Object) object.Object {
    return if (obj == .@"return") obj.@"return".value.* else obj;
}

// sometimes i do not know how to write beautiful code ...
fn evalAssignment(allocator: std.mem.Allocator, assig: ast.AssignmentExpression, env: *object.Environment) !object.Object {
    switch (assig.name.*) {
        .identifier => |ident| {
            var evaluated = try eval(allocator, .{ .expression = assig.value.* }, env);

            switch (assig.token.type) {
                .@"+=", .@"-=", .@"*=", .@"/=" => {
                    if (env.get(ident.value)) |*current| {
                        var result = try evalInfixExpression(allocator, assig.operator, @constCast(current), &evaluated, env);
                        _ = try env.set(ident.value, result);
                        return NULL;
                    }
                    return error.VariableNotDeclared;
                },

                .@"=" => {
                    if (env.get(ident.value)) |_| {
                        _ = try env.set(ident.value, evaluated);
                        return NULL;
                    }
                    return error.VariableNotDeclared;
                },

                else => return error.UnknowOperator,
            }
        },

        .index_expression => |exp| {
            const index_obj = try eval(allocator, .{ .expression = exp.index.* }, env);
            const index = index_obj.integer.value;
            const evaluated = try eval(allocator, .{ .expression = assig.value.* }, env);
            const var_name = exp.left.identifier.value;
            if (env.get(var_name)) |*current| {
                const uindex = @intCast(usize, index);
                switch (current.array.elements[uindex]) {
                    .integer => {
                        var element = &current.array.elements[uindex];
                        switch (assig.token.type) {
                            .@"=", .@"+=", .@"-=", .@"*=", .@"/=" => _ = try evalInfixExpression(allocator, assig.token.literal, element, @constCast(&evaluated), env),
                            else => return error.UnknowOperator,
                        }
                    },
                    .string => {
                        var element = &current.array.elements[uindex];
                        switch (assig.token.type) {
                            .@"=", .@"+=" => _ = try evalInfixExpression(allocator, assig.token.literal, element, @constCast(&evaluated), env),
                            else => return error.UnknowOperator,
                        }
                    },
                    else => return error.NotSuportedOperation,
                }
            }
            return NULL;
        },
        else => return error.AssignmentExpressionNotDefined,
    }
}

fn evalHashExpression(
    allocator: std.mem.Allocator,
    hash_exp: *const std.AutoHashMap(*ast.Expression, *ast.Expression),
    env: *object.Environment,
) !std.AutoHashMap(*object.Object, *object.Object) {
    var hash_obj = std.AutoHashMap(*object.Object, *object.Object).init(allocator);
    errdefer hash_obj.deinit();

    var iter0 = hash_exp.iterator();

    while (iter0.next()) |hash| {
        var keyval = try allocator.alloc(object.Object, 2);
        keyval[0] = try eval(allocator, .{ .expression = hash.key_ptr.*.* }, env);
        keyval[1] = try eval(allocator, .{ .expression = hash.value_ptr.*.* }, env);

        try env.allocated_obj.append(keyval);
        try hash_obj.put(&keyval[0], &keyval[1]);
    }

    try env.allocated_hash.append(hash_obj);

    return hash_obj;
}

fn evalExpression(allocator: std.mem.Allocator, exps: []ast.Expression, env: *object.Environment) ![]object.Object {
    // TODO 144
    var result = std.ArrayList(object.Object).init(allocator);
    errdefer result.deinit();
    for (exps) |e| {
        var evaluated = try eval(allocator, .{ .expression = e }, env);
        try result.append(evaluated);
    }
    var result_owned = try result.toOwnedSlice();
    try env.allocated_obj.append(result_owned);

    return result_owned;
}

fn evalProgram(allocator: std.mem.Allocator, stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(allocator, .{ .statement = statement }, env);
        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => {},
        }
    }
    return result;
}

fn evalBlockStatement(allocator: std.mem.Allocator, stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(allocator, .{ .statement = statement }, env);
        switch (result) {
            .@"return" => return result,
            else => {},
        }
    }
    return result;
}

fn evalStatement(allocator: std.mem.Allocator, stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(allocator, .{ .statement = statement }, env);
        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => {},
        }
    }
    return result;
}

fn evalIfExpression(allocator: std.mem.Allocator, ie: *const ast.IfExpression, env: *object.Environment) anyerror!object.Object {
    const condition = try eval(allocator, .{ .expression = ie.condition.* }, env);

    if (condition != .boolean) return NULL;

    if (condition.boolean.value) {
        // { block statements if}
        return try eval(allocator, .{ .statement = .{ .block_statement = ie.consequence } }, env);
    } else if (ie.alternative) |alternative| {
        // { block statement else }
        return try eval(allocator, .{ .statement = .{ .block_statement = alternative } }, env);
    }

    return NULL;
}

fn evalInfixExpression(allocator: std.mem.Allocator, op: []const u8, left: *object.Object, right: *object.Object, env: *object.Environment) !object.Object {
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
        } else NULL;
    }

    // bool bool operation
    if (right.objType() == .boolean and left.objType() == .boolean) {
        return if (eql(u8, op, "=="))
            .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } }
        else if (eql(u8, op, "!="))
            .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } }
        else
            NULL;
    }

    // string string op
    if (right.objType() == .string and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{s}{s}", .{ left.string.value, right.string.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{s}{s}", .{ left.string.value, right.string.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else NULL;
    }

    if (right.objType() == .integer and left.objType() == .string) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{s}{any}", .{ left.string.value, right.integer.value });
            try env.allocated_str.append(new_string);
            left.string.value = new_string;
            break :b left.*;
        } else NULL;
    }

    if (right.objType() == .string and left.objType() == .integer) {
        return if (eql(u8, op, "+")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
            break :b .{ .string = .{ .value = new_string } };
        } else if (eql(u8, op, "+=")) b: {
            var new_string = try std.fmt.allocPrint(allocator, "{any}{s}", .{ left.integer.value, right.string.value });
            try env.allocated_str.append(new_string);
            // left.string.value = new_string;
            left.* = .{ .string = .{ .value = new_string } };
            break :b left.*;
        } else NULL;
    }

    return NULL;
}

fn evalPrefixExpression(op: []const u8, right: object.Object) object.Object {
    // TODO: op deve ser um u8
    if (right.objType() == .boolean and op[0] == '!') {
        return .{ .boolean = .{ .value = !right.boolean.value } };
    }

    if (right.objType() == .integer and op[0] == '-') {
        return .{ .integer = .{ .value = -right.integer.value } };
    }

    return NULL;
}

fn evalIndexExpression(left: *const object.Object, index: *const object.Object) !object.Object {
    return if (left.objType() == .array and index.objType() == .integer)
        evalArrayIndexExpression(left, index)
    else if (left.objType() == .hash)
        evalHashIndexExpression(left, index)
    else
        error.IndexOPNotSupported;
}

fn evalHashIndexExpression(hash: *const object.Object, key: *const object.Object) !object.Object {
    const hash_obj = hash.hash;
    var k = @constCast(key);

    var iter = hash_obj.elements.iterator();

    // TODO: implement a better logic
    while (iter.next()) |h| {
        if (h.key_ptr.*.* == .string and k.* == .string) {
            if (std.mem.eql(u8, h.key_ptr.*.*.string.value, k.*.string.value)) return h.value_ptr.*.*;
            return NULL;
        }

        if (std.meta.eql(h.key_ptr.*.*, k.*)) {
            return h.value_ptr.*.*;
        }
    }
    return NULL;
}

fn evalArrayIndexExpression(array: *const object.Object, index: *const object.Object) !object.Object {
    const arr_obj = array.array;

    const idx = @intCast(usize, index.integer.value);

    const max = arr_obj.elements.len - 1;

    if (idx < 0 or idx > max) {
        return NULL;
    }

    return arr_obj.elements[idx];
}

fn evalForLoopExpression(allocator: std.mem.Allocator, fl: *const ast.ForLoopExpression, env: *object.Environment) !object.Object {
    var rt = object.Boolean{ .value = true };
    while (true) {
        var condition = try eval(allocator, .{ .expression = fl.condition.* }, env);

        if (condition != .boolean) condition = .{ .boolean = .{ .value = false } };

        if (condition.boolean.value) {
            var rt2 = try eval(
                allocator,
                .{ .statement = .{ .block_statement = fl.consequence } },
                env,
            );

            if (rt2.objType() == .@"return") return rt2;
        } else break;
    }
    return .{ .boolean = rt };
}

fn evalForLoopRangeExpression(allocator: std.mem.Allocator, fl: *const ast.ForLoopRangeExpression, env: *object.Environment) !object.Object {
    var iterabol = try eval(allocator, .{ .expression = fl.iterable.* }, env);

    std.debug.print("{}", .{iterabol.objType()});

    var permit0 = std.ArrayList([]const u8).init(allocator);
    defer permit0.deinit();
    try permit0.append(fl.ident);
    if (fl.index) |index|
        try permit0.append(index);

    const permit = try permit0.toOwnedSlice();
    defer allocator.free(permit);
    var child_env = object.Environment.newTemporaryScope(env, permit);
    defer child_env.deinit();
    var info = try iterabol.next();
    var element = info.element;
    var idx = info.index;
    var ok = info.ok;

    while (ok) {
        _ = try child_env.set(fl.ident, element);

        if (fl.index) |index|
            _ = try child_env.set(index, idx);

        var block_eval = try eval(allocator, .{ .statement = .{ .block_statement = fl.body } }, &child_env);

        if (block_eval.objType() == .@"return") return block_eval;

        info = try iterabol.next();
        element = info.element;
        idx = info.index;
        ok = info.ok;
    }

    return NULL;
}
