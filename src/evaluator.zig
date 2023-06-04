// TODO: implement errors for parsing and evaluation
const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;

pub fn eval(node: ast.Node, env: *object.Environment) anyerror!object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .boolean => |b| .{ .boolean = .{ .value = b.value } },
                .integer_literal => |i| .{ .integer = .{ .value = i.value } },
                .prefix_expression => |pre| blk: {
                    const right = try eval(.{ .expression = pre.right.* }, env);
                    break :blk evalPrefixExpression(pre.operator, right);
                },
                .infix_expression => |pre| blk: {
                    const left = try eval(.{ .expression = pre.left.* }, env);
                    const right = try eval(.{ .expression = pre.right.* }, env);
                    break :blk evalInfixExpression(pre.operator, left, right);
                },
                .if_expression => |*if_exp| try evalIfExpression(if_exp, env),

                .identifier => |iden| env.get(iden.value).?,
                .function_literal => |func| .{
                    .function = .{
                        .parameters = func.parameters,
                        .body = &func.body,
                        .env = env,
                    },
                },
                .call_expression => |call| blk: {
                    var func = eval(.{ .function_literal = .{ .function = call.function } }, env);

                    var args = evalExpression(call.arguments, env);

                    if (args.len == 1) return args[0];

                    break :blk applyFunction(func, args);
                },
                // else => createNull(),
            };
        },
        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| try evalProgram(program.statements.items, env),
                .block_statement => |block| try evalBlockStatement(block.statements, env),
                .expression_statement => |exp_stmt| eval(.{ .expression = exp_stmt.expression.* }, env),
                .var_statement => |var_stmt| blk: {
                    var val = try eval(.{ .expression = var_stmt.value.? }, env);
                    break :blk try env.set(var_stmt.name.value, val);
                },
                .return_statement => |ret_stmt| blk: {
                    const exp = if (ret_stmt.value) |exp| exp else return createNull();
                    break :blk .{ .@"return" = .{ .value = &(try eval(.{ .expression = exp }, env)) } };
                },
            };
        },
    }
}

fn applyFunction(func: object.Object, args: []object.Object) object.Object {
    _ = args;
    _ = func;
    // TODO 147
}

fn evalExpression(exps: []ast.Expression, env: *object.Environment) []object.Object {
    // TODO 144
    _ = env;
    _ = exps;
}

fn evalProgram(stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(.{ .statement = statement }, env);
        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => {},
        }
    }
    return result;
}

fn evalBlockStatement(stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(.{ .statement = statement }, env);
        switch (result) {
            .@"return" => return result,
            else => {},
        }
    }
    return result;
}

fn evalStatement(stmts: []ast.Statement, env: *object.Environment) anyerror!object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = try eval(.{ .statement = statement }, env);
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

fn evalIfExpression(ie: *const ast.IfExpression, env: *object.Environment) anyerror!object.Object {
    const condition = try eval(.{ .expression = ie.condition.* }, env);

    if (condition != .boolean) return createNull();

    if (condition.boolean.value) {
        // { block statements if}
        return try eval(.{ .statement = .{ .block_statement = ie.consequence } }, env);
    } else if (ie.alternative) |alternative| {
        // { block statement else }
        return try eval(.{ .statement = .{ .block_statement = alternative } }, env);
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
            '=' => if (op[1] == '=') .{ .boolean = .{ .value = left.integer.value == right.integer.value } } else createNull(),
            '!' => if (op[1] == '=') .{ .boolean = .{ .value = left.integer.value != right.integer.value } } else createNull(),
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

test "int" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("-69 - 1");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    var node = ast.Node{ .expression = exp.* };

    var obj = try eval(node, &env);

    try std.testing.expect(obj.integer.value == -69 - 1);
}

test "bool" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("!!true == true");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    var node = ast.Node{ .expression = exp.* };

    var env = object.Environment.init(allocator);
    defer env.deinit();
    var obj = try eval(node, &env);

    try std.testing.expect(obj.boolean.value == (!!true == true));
}

test "infix int" {
    const allocator = std.testing.allocator;

    const tests = [_]struct { input: []const u8, value: i64 }{
        .{ .input = "10 + 10", .value = 20 },
        .{ .input = "10 - 10", .value = 0 },
        .{ .input = "10 * 10", .value = 100 },
        .{ .input = "10 / 10", .value = 1 },
        .{ .input = "10 + 10 * 10", .value = 110 },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        var node = ast.Node{ .expression = exp.* };

        var env = object.Environment.init(allocator);
        defer env.deinit();
        var obj = try eval(node, &env);

        try std.testing.expect(obj.integer.value == x.value);
    }
}
test "infix bool" {
    const allocator = std.testing.allocator;

    const tests = [_]struct { input: []const u8, value: bool }{
        .{ .input = "10 == 10", .value = true },
        .{ .input = "10 != 10", .value = false },
        .{ .input = "10 > 10", .value = false },
        .{ .input = "10 > 1", .value = true },
        .{ .input = "10 < 10", .value = false },
        .{ .input = "10 < 1", .value = false },
        .{ .input = "(10 < 1) == true", .value = false },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        var node = ast.Node{ .expression = exp.* };

        var env = object.Environment.init(allocator);
        defer env.deinit();
        var obj = try eval(node, &env);
        try std.testing.expect(obj.boolean.value == x.value);
    }
}

test "if-else expression" {
    const allocator = std.testing.allocator;

    const tests = [_]struct { input: []const u8, value: ?i64 }{
        .{ .input = "if (true)  { 10 }", .value = 10 },
        .{ .input = "if (false) { 10 } else { 0 }", .value = 0 },
        .{ .input = "if (1 < 2) { 10 } else { 0 }", .value = 10 },
        .{ .input = "if (1 > 2) { 10 } else { 0 }", .value = 0 },
        .{ .input = "if (false) { false }", .value = null },
        .{ .input = 
        \\if (if (true) { true } else { false }) { if (false) { 0 } else { 2 } } else { 0 }
        , .value = 2 },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        var node = ast.Node{ .expression = exp.* };

        var env = object.Environment.init(allocator);
        defer env.deinit();
        var obj = try eval(node, &env);

        switch (obj) {
            .integer => |int| try std.testing.expect(int.value == x.value),
            .null => |nil| try std.testing.expect(nil.value == x.value),
            else => return error.UnexpectedObj,
        }
    }
}

test "return" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(
        \\if (10 > 1) { if (10 > 1) { return 10; } return 1; }
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var stmt = program.statements.items[0];

    const exp = stmt.expression_statement.expression;

    var node = ast.Node{ .expression = exp.* };

    var env = object.Environment.init(allocator);
    defer env.deinit();
    var obj = try eval(node, &env);

    var int = obj.@"return".value.integer;

    try std.testing.expect(int.value == 10);
}

test "env" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(
        \\var x = 10; 
        \\var y = 2 * x + if (true) {10} else {-1}; 
        \\return y;
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(.{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 30);
}
test "function " {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(
        \\var foo = fn(x) { 
        \\  y = 2 * x + if (true) {10} else {-1}; 
        \\  return y;
        \\};
        \\foo(10);
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(.{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 30);
}
