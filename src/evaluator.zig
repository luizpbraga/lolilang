// TODO: implement errors for parsing and evaluation
const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;

pub fn eval(node: ast.Node) object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .boolean => |b| .{ .boolean = .{ .value = b.value } },
                .integer_literal => |i| .{ .integer = .{ .value = i.value } },
                // .identifier => {},
                // .function_literal => {},
                .prefix_expression => |pre| blk: {
                    const right = eval(.{ .expression = pre.right.* });
                    break :blk evalPrefixExpression(pre.operator, right);
                },
                .infix_expression => |pre| blk: {
                    const left = eval(.{ .expression = pre.left.* });
                    const right = eval(.{ .expression = pre.right.* });
                    break :blk evalInfixExpression(pre.operator, left, right);
                },
                .if_expression => |*if_exp| evalIfExpression(if_exp),
                // .call_expression => {},
                else => createNull(),
            };
        },
        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| evalProgram(program.statements.items),
                .block_statement => |block| evalBlockStatement(block.statements),
                .expression_statement => |exp_stmt| eval(.{ .expression = exp_stmt.expression.* }),
                // .var_statement => |var_stmt| var_stmt,
                .return_statement => |ret_stmt| blk: {
                    const exp = if (ret_stmt.value) |exp| exp else return createNull();
                    break :blk .{ .@"return" = .{ .value = &eval(.{ .expression = exp }) } };
                },
                else => createNull(),
            };
        },
    }
}

fn evalProgram(stmts: []ast.Statement) object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = eval(.{ .statement = statement });

        switch (result) {
            .@"return" => |ret| return ret.value.*,
            else => {},
        }
    }
    return result;
}

fn evalBlockStatement(stmts: []ast.Statement) object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = eval(.{ .statement = statement });

        switch (result) {
            .@"return" => return result,
            else => {},
        }
    }
    return result;
}

fn evalStatement(stmts: []ast.Statement) object.Object {
    var result: object.Object = undefined;
    for (stmts) |statement| {
        result = eval(.{ .statement = statement });

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

fn evalIfExpression(ie: *const ast.IfExpression) object.Object {
    const condition = eval(.{ .expression = ie.condition.* });

    if (condition != .boolean) return createNull();

    if (condition.boolean.value) {
        // { block statements if}
        return eval(.{ .statement = .{ .block_statement = ie.consequence } });
    } else if (ie.alternative) |alternative| {
        // { block statement else }
        return eval(.{ .statement = .{ .block_statement = alternative } });
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
    //
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

    var stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    var node = ast.Node{ .expression = exp.* };

    var obj = eval(node);

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

    var obj = eval(node);

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

        var obj = eval(node);

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

        var obj = eval(node);
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

        var obj = eval(node);

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

    var obj = eval(node);

    var int = obj.@"return".value.integer;

    try std.testing.expect(int.value == 10);
}
