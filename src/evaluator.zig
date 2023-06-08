// TODO: implement errors for parsing and evaluation
const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;

pub fn eval(allocator: std.mem.Allocator, node: ast.Node, env: *object.Environment) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .boolean => |b| .{ .boolean = .{ .value = b.value } },
                .integer_literal => |i| .{ .integer = .{ .value = i.value } },
                .string_literal => |i| .{ .string = .{ .value = i.value } },
                .array_literal => |array| bkl: {
                    var elements = try evalExpression(allocator, array.elements, env);
                    break :bkl .{ .array = .{ .elements = elements } };
                },
                .prefix_expression => |pre| blk: {
                    const right = try eval(allocator, .{ .expression = pre.right.* }, env);
                    break :blk evalPrefixExpression(pre.operator, right);
                },
                .infix_expression => |pre| blk: {
                    const left = try eval(allocator, .{ .expression = pre.left.* }, env);
                    const right = try eval(allocator, .{ .expression = pre.right.* }, env);
                    break :blk evalInfixExpression(pre.operator, left, right);
                },
                .if_expression => |*if_exp| try evalIfExpression(allocator, if_exp, env),

                .identifier => |iden| env.get(iden.value).?,

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
                    const left = try eval(allocator, .{ .expression = idx.left.* }, env);
                    var index = try eval(allocator, .{ .expression = idx.index.* }, env);
                    break :blk evalIndexExpression(left, index);
                },
            };
        },

        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| try evalProgram(allocator, program.statements.items, env),

                .block_statement => |block| try evalBlockStatement(allocator, block.statements, env),

                .expression_statement => |exp_stmt| eval(allocator, .{ .expression = exp_stmt.expression.* }, env),

                .var_statement => |var_stmt| blk: {
                    var val = try eval(allocator, .{ .expression = var_stmt.value.? }, env);
                    break :blk try env.set(var_stmt.name.value, val);
                },

                .return_statement => |ret_stmt| blk: {
                    const exp = if (ret_stmt.value) |exp| exp else return createNull();
                    break :blk .{
                        .@"return" = .{ .value = &(try eval(allocator, .{ .expression = exp }, env)) },
                    };
                },
            };
        },
    }
}

fn applyFunction(allocator: std.mem.Allocator, func: object.Object, args: []object.Object) !object.Object {
    // TODO 147
    var function = func.function;
    var extended_env = try extendFunctionEnv(&function, args);
    defer extended_env.deinit();
    var evaluated = try eval(allocator, .{ .statement = .{ .block_statement = function.body } }, &extended_env);
    return unwrapReturnValue(evaluated);
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

fn evalAssignment(allocator: std.mem.Allocator, assig: ast.AssignmentExpression, env: *object.Environment) !object.Object {
    var evaluated = try eval(allocator, .{ .expression = assig.value.* }, env);

    return switch (assig.token.type) {
        .@"+=", .@"-=", .@"*=", .@"/=" => blk1: {
            if (env.get(assig.name.value)) |current| {
                var result = evalInfixExpression(assig.operator, current, evaluated);
                _ = try env.set(assig.name.value, result);
                break :blk1 result;
            }

            return error.VariableNotDeclared;
        },
        .@"=" => blk2: {
            if (env.get(assig.name.value)) |_| {
                _ = try env.set(assig.name.value, evaluated);
                break :blk2 evaluated;
            }

            return error.VariableNotDeclared;
        },

        else => error.UnknowOperator,
    };
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

fn createNull() object.Object {
    return .{ .null = object.Null{} };
}

fn evalIfExpression(allocator: std.mem.Allocator, ie: *const ast.IfExpression, env: *object.Environment) anyerror!object.Object {
    const condition = try eval(allocator, .{ .expression = ie.condition.* }, env);

    if (condition != .boolean) return createNull();

    if (condition.boolean.value) {
        // { block statements if}
        return try eval(allocator, .{ .statement = .{ .block_statement = ie.consequence } }, env);
    } else if (ie.alternative) |alternative| {
        // { block statement else }
        return try eval(allocator, .{ .statement = .{ .block_statement = alternative } }, env);
    }

    return createNull();
}

fn evalInfixExpression(op: []const u8, left: object.Object, right: object.Object) object.Object {
    if (right.objType() == .integer and left.objType() == .integer) {
        return switch (op[0]) {
            '+' => .{ .integer = .{ .value = left.integer.value + right.integer.value } },
            '-' => .{ .integer = .{ .value = left.integer.value - right.integer.value } },
            '*' => .{ .integer = .{ .value = left.integer.value * right.integer.value } },
            '/' => .{ .integer = .{ .value = @divFloor(left.integer.value, right.integer.value) } },
            '>' => .{ .boolean = .{ .value = left.integer.value > right.integer.value } },
            '<' => .{ .boolean = .{ .value = left.integer.value < right.integer.value } },
            '=' => if (op.len != 1 and op[1] == '=') .{ .boolean = .{ .value = left.integer.value == right.integer.value } } else createNull(),
            '!' => if (op.len != 1 and op[1] == '=') .{ .boolean = .{ .value = left.integer.value != right.integer.value } } else createNull(),
            // '/' => left.integer.value / right.integer.value,
            else => createNull(),
        };
    }

    if (right.objType() == .boolean and left.objType() == .boolean) {
        return switch (op[0]) {
            '=' => if (op[1] == '=') .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } } else createNull(),
            '!' => if (op[1] == '=') .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } } else createNull(),
            else => createNull(),
        };
    }

    if (right.objType() == .string and left.objType() == .string) {
        var buff: [100]u8 = undefined;
        var new_string = std.fmt.bufPrint(&buff, "{s}{s}", .{ left.string.value, right.string.value }) catch "";
        return switch (op[0]) {
            '+' => .{ .string = .{ .value = new_string } },
            else => createNull(),
        };
    }

    return createNull();
}

fn evalPrefixExpression(op: []const u8, right: object.Object) object.Object {
    // TODO: op deve ser um u8
    if (right.objType() == .boolean and op[0] == '!') {
        return .{ .boolean = .{ .value = !right.boolean.value } };
    }

    if (right.objType() == .integer and op[0] == '-') {
        return .{ .integer = .{ .value = -right.integer.value } };
    }

    return createNull();
}

fn evalIndexExpression(left: object.Object, index: object.Object) !object.Object {
    return if (left.objType() == .array and index.objType() == .integer)
        evalArrayIndexExpression(left, index)
    else
        error.IndexOPNotSupported;
}

fn evalArrayIndexExpression(array: object.Object, index: object.Object) !object.Object {
    const arr_obj = array.array;

    const idx = @intCast(usize, index.integer.value);

    const max = arr_obj.elements.len - 1;

    if (idx < 0 or idx > max) {
        return createNull();
    }

    return arr_obj.elements[idx];
}
