const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;
const Loli = @import("loli.zig");

pub fn eval(node: ast.Node) object.Object {
    switch (node) {
        .expression => |exp| {
            return switch (exp) {
                .boolean => |b| .{ .boolean = object.Boolean{ .value = b.value } },
                .integer_literal => |i| .{ .integer = object.Integer{ .value = i.value } },
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
                // .if_expression => {},
                // .call_expression => {},
                else => createNull(),
            };
        },
        .statement => |stmt| {
            return switch (stmt) {
                .program_statement => |program| blk: {
                    var result: object.Object = undefined;
                    for (program.statements.items) |statement|
                        result = eval(.{ .statement = statement });
                    break :blk result;
                },
                .expression_statement => |exp_stmt| eval(.{ .expression = exp_stmt.expression.* }),
                // .var_statement => |var_stmt| var_stmt,
                // .return_statement => |ret_stmt| ret_stmt,
                else => createNull(),
            };
        },
    }
}

fn createNull() object.Object {
    return .{ .null = object.Null{} };
}
fn evalInfixExpression(op: []const u8, left: object.Object, right: object.Object) object.Object {
    if (right.objType() == .integer and left.objType() == .integer) {
        switch (op[0]) {
            '+' => return .{ .integer = .{ .value = left.integer.value + right.integer.value } },
            '-' => return .{ .integer = .{ .value = left.integer.value - right.integer.value } },
            '*' => return .{ .integer = .{ .value = left.integer.value * right.integer.value } },
            '>' => return .{ .integer = .{ .value = left.integer.value > right.integer.value } },
            '<' => return .{ .integer = .{ .value = left.integer.value < right.integer.value } },
            '=' => if (op[1] == '=') return .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } },
            '!' => if (op[1] == '=') return .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } },
            // '/' => left.integer.value / right.integer.value,
            else => return createNull(),
        }

        if (right.objType() == .boolean and left.objType() == .boolean) {
            switch (op[0]) {
                '=' => if (op[1] == '=') return .{ .boolean = .{ .value = left.boolean.value == right.boolean.value } },
                '!' => if (op[1] == '=') return .{ .boolean = .{ .value = left.boolean.value != right.boolean.value } },
                else => return createNull(),
            }
        }
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
    std.debug.print("\n>> {s}\n", .{try obj.inspect()});

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
    std.debug.print("\n>> {s}\n", .{try obj.inspect()});

    try std.testing.expect(obj.boolean.value == (!!true == true));
}
