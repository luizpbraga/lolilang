const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const Type = @import("Token.zig").Type;

const talloc = std.testing.allocator;

fn testIdentifier(exp: *ast.Expression, value: anytype) !void {
    if (@TypeOf(value) == bool) {
        const ident = exp.boolean;

        if (!ident.value == value)
            return error.UnexpectedValue;
        return;
    }

    const ident = exp.identifier;

    if (!std.mem.eql(u8, ident.value, value)) {
        std.log.err("find {s} expected {s}\n", .{ ident.value, value });
        return error.UnexpectedValue;
    }

    // if (!std.mem.eql(u8, ident.tokenLiteral(), value)) {
    //     std.log.err("find {s} expected {s}\n", .{ ident.tokenLiteral(), value });
    //     return error.UnexpectedValue;
    // }
}

fn testIntegerLiteral(exp: *ast.Expression, value: i64) !void {
    const integer = exp.integer;

    if (integer.value != value) {
        std.log.err("expect {} got {};", .{ integer.value, value });
        return error.UnexpectedValue;
    }

    // var buff: [10]u8 = undefined;
    // const value_str = try std.fmt.bufPrint(&buff, "{d}", .{value});
    // if (!std.mem.eql(u8, integer.tokenLiteral(), value_str)) {
    //     std.log.err("expect {s} got {s};", .{ integer.tokenLiteral(), value_str });
    //     return error.UnexpectedValue;
    // }
}

fn testLiteralExpression(exp: *ast.Expression, expected: anytype) !void {
    if (@TypeOf(expected) == i64 or @TypeOf(expected) == comptime_int) {
        try testIntegerLiteral(exp, expected);
    } else {
        try testIdentifier(exp, expected);
    }
}

fn testInfixExpression(exp: *ast.Expression, left: anytype, op: []const u8, right: anytype) !void {
    const opExp = exp.infix;

    try testLiteralExpression(opExp.left, left);

    if (!std.mem.eql(u8, op, @tagName(opExp.operator)))
        return error.UnexpectedOP;

    try testLiteralExpression(opExp.right, right);
}
//
// test "switch" {
//     var lexer = Lexer.init(
//         \\con x = 0
//         \\switch x {
//         \\  0    => { 0 },
//         \\  1    => { 1 },
//         \\  else => { 2 }
//         \\}
//         \\print(x)
//     );
//     var parser = Parser.new(talloc, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     if (program.statements.items.len != 3) {
//         std.log.err("len: {d}", .{program.statements.items.len});
//         return error.NotEnoughStatements;
//     }
// }
//
// test "for in loop" {
//     for ([_][]const u8{"for i,idx in list { const y = 10 }"}) |x| {
//         var lexer = Lexer.init(x);
//         var parser = Parser.new(talloc, &lexer);
//         defer parser.deinit();
//
//         const program = try parser.parseProgram();
//
//         if (program.statements.items.len != 1) {
//             std.log.err("len: {d}", .{program.statements.items.len});
//             return error.NotEnoughStatements;
//         }
//         const stmt = program.statements.items[0].exp_statement.expression.forloop_range;
//
//         const con = stmt.body;
//
//         const v = con.statements[0].@"var";
//         _ = v;
//     }
// }
//
// test "for loop" {
//     for ([_][]const u8{"for { const y = 10 }"}) |x| {
//         var lexer = Lexer.init(x);
//         var parser = Parser.new(talloc, &lexer);
//         defer parser.deinit();
//
//         const program = try parser.parseProgram();
//
//         if (program.statements.items.len != 1) {
//             std.log.err("len: {d}", .{program.statements.items.len});
//             return error.NotEnoughStatements;
//         }
//         const stmt = program.statements.items[0].exp_statement.expression.forloop_expression;
//
//         const con = stmt.consequence;
//
//         const v = con.statements[0].@"var";
//         _ = v;
//     }
// }

// test "hash array" {
//     var lexer = Lexer.init(
//         \\{ true: false, 1: 0 }
//     );
//     var parser = Parser.new(talloc, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     if (program.statements.items.len != 1) {
//         std.log.err("len: {d}", .{program.statements.items.len});
//         return error.NotEnoughStatements;
//     }
//     const stmt = program.statements.items[0].exp_statement.expression.hash.pairs;
//     _ = stmt;
// }
//
test "method call" {
    var lexer = Lexer.init(
        \\con str = "ola mundo"
        \\con y = str.len + 1
        \\con x = 10
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 3) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[1].con;
    _ = stmt;
    // const len = stmt.value.?.method.method.value;
    // _ = len;
}

// // test "con/const block" {
// //     var lexer = Lexer.init(
// //         \\con {
// //         \\  x = 10 + 1
// //         \\  y = "ola"
// //         \\}
// //         \\
// //         \\con {
// //         \\  x = 10
// //         \\  y = "ola"
// //         \\  z = "ola" + "mundo"
// //         \\  w = {1,2,3,4}
// //         \\}
// //     );
// //     var parser = Parser.new(talloc, &lexer);
// //     defer parser.deinit();
// //
// //     const program = try parser.parseProgram();
// //
// //     if (program.statements.items.len != 2) {
// //         std.log.err("len: {d}", .{program.statements.items.len});
// //         return error.NotEnoughStatements;
// //     }
// //     // const stmt = program.statements.items[0].exp_statement;
// //
// //     // const exp = stmt.expression.call;
// //     // _ = exp;
// // }
// //
test "Function Call 3" {
    var lexer = Lexer.init("fn(){ \"1\" + \"2\" }();");
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }
    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.call;
    _ = exp;
}
test "Function Call 2" {
    var lexer = Lexer.init("fn(x, y){ 1 + 2 }(1,2);");
    var parser = Parser.new(talloc, &lexer);

    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }
    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.call;

    if (exp.arguments.len != 2)
        return error.WrongNumberOfArguments;

    try testLiteralExpression(exp.arguments[0], 1);
    try testLiteralExpression(exp.arguments[1], 2);

    // try testInfixExpression(&exp.arguments[1], 1, "+", 2);
}

test "parse String" {
    var lexer = Lexer.init(
        \\"bruh"
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;
    const exp = stmt.expression.string;

    try std.testing.expect(std.mem.eql(u8, "bruh", exp.value));
}

test "array literal" {
    var lexer = Lexer.init(
        \\[ 1, true, "Ola", fn(){ return 0; }() ];
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;
    const exp = stmt.expression.array;

    try std.testing.expect(exp.elements.len == 4);
    // TODO
    // try std.testing.expect(exp.elements[0] == .integer);
    // try std.testing.expect(exp.elements[1] == .boolean);
    // try std.testing.expect(exp.elements[2] == .string);
    // try std.testing.expect(exp.elements[3] == .call);
}

test "Index" {
    var lexer = Lexer.init(
        \\myArray[1];
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;
    const exp = stmt.expression.index;

    // TODO: fix
    try testIdentifier(exp.left, "myArray");
    try testIntegerLiteral(exp.index, 1);
}

test "Function Call" {
    var lexer = Lexer.init("add(x, 1 + 2);");
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }
    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.call;

    if (exp.arguments.len != 2)
        return error.WrongNumberOfArguments;

    try testLiteralExpression(exp.arguments[0], "x");
    try testInfixExpression(exp.arguments[1], 1, "+", 2);

    try testIdentifier(exp.function, "add");
}

test "Function Literal" {
    const tests = [_]struct {
        input: []const u8,
        len: usize,
    }{
        // .{ .input = "fn(){};", .len = 0 },
        .{ .input = "fn(x, y){ x + y; };", .len = 2 },
        // .{ .input = "fn(x, y, z){};", .len = 3 },
        // .{ .input = "fn(x, y, z){ const x = 10; };", .len = 3 },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = Parser.new(talloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        const stmt = program.statements.items[0].exp_statement;

        const func = stmt.expression.function;

        if (func.parameters.len != x.len) {
            std.log.err("\n{s}\n", .{x.input});
            return error.WrongParmeterLengh;
        }

        if (!std.mem.eql(u8, func.parameters[0].value, "x"))
            return error.UnexpectedValue;

        if (func.body.statements.len != 1)
            return error.WrongFunctionBody;

        const body_stmt = func.body.statements[0].exp_statement;

        try testInfixExpression(body_stmt.expression, "x", "+", "y");
    }
}

//// test "con x = (((( 10 ))))" {
////     var lexer = Lexer.init("con x = 10;");
////     var parser =Parser.new(talloc, &lexer);

////     const program = try parser.parseProgram();

////     if (program.statements.items.len != 1) {
////         std.log.err("len: {d}", .{program.statements.items.len});
////         return error.NotEnoughStatements;
////     }

////     const stmt = program.statements.items[0].@"var";
////     std.debug.print("\n{s}\n", .{stmt.tokenLiteral()});
////     std.debug.print("\n{s} {s}\n", .{ stmt.tokenLiteral(), stmt.name.value });
//// }
test "assignment list (=)" {
    var lexer = Lexer.init(
        \\con x = [1,2,3];
        \\x [0 + 1] = 0
        \\x[0];
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();
    _ = program;

    // std.debug.print("{}", .{program.statements.items[2].exp_statement});
}

test "assignment expression (=)" {
    var lexer = Lexer.init(
        \\con x = 10;
        \\x = if (x == x) { 2 * x } else { null };
        \\x;
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();
    _ = program;

    // std.debug.print("{}", .{program.statements.items[2].exp_statement});
}

test "assignment statement (+=)" {
    var lexer = Lexer.init(
        \\con x = 10;
        \\x += if (x == x) { 2 * x } else { null };
        \\x;
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();
    _ = program;

    // std.debug.print("{}", .{program.statements.items[2].exp_statement});
}
test "If Else Expression" {
    var lexer = Lexer.init(
        \\return if x < y {x} else {y};
    );
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const ret = program.statements.items[0].@"return".value;
    const exp = ret.@"if";

    try testInfixExpression(exp.condition, "x", "<", "y");

    if (exp.consequence.statements.len != 1)
        return error.ConsequenceIs1Statement;

    const consequence = exp.consequence.statements[0].exp_statement;

    try testIdentifier(consequence.expression, "x");

    if (exp.alternative == null)
        return error.AlternativeStatementsWasNotNull;

    const alternative = exp.alternative.?.statements[0].exp_statement;
    try testIdentifier(alternative.expression, "y");
}

test "If Expression" {
    var lexer = Lexer.init("return if (x < y) { x };");
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].@"return".value;

    const exp = stmt.@"if";

    try testInfixExpression(exp.condition, "x", "<", "y");

    if (exp.consequence.statements.len != 1)
        return error.ConsequenceIs1Statement;

    const consequence = exp.consequence.statements[0].exp_statement;
    _ = consequence;

    if (exp.alternative != null)
        return error.AlternativeStatementsWasNotNull;
}

test "Group Exp" {
    var lexer = Lexer.init("((-(5 + 5) * 5) == 10) != !true");
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;
    const exp = stmt.expression;
    _ = exp;
}

test "eval Boolean" {
    var lexer = Lexer.init("false");
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;
    const exp = stmt.expression;

    try std.testing.expect(false == exp.boolean.value);
}

test "Boolean" {
    const tests = [_]struct {
        l: bool,
        r: bool,
        op: []const u8,
        input: []const u8,
    }{
        .{ .input = "true == true", .l = true, .r = true, .op = "==" },
        .{ .input = "false == false", .l = false, .r = false, .op = "==" },
        .{ .input = "false != true", .l = false, .r = true, .op = "!=" },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = Parser.new(talloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        const stmt = program.statements.items[0].exp_statement;

        const exp = stmt.expression.infix;

        const operator = exp.operator;

        if (!std.mem.eql(u8, @tagName(operator), x.op))
            return error.UnexpectedOP;

        const left = exp.left.boolean;
        const right = exp.right.boolean;

        if (left.value != x.l or right.value != x.r)
            return error.UnexpectedValue;
    }
}

test "Parse Infix OP " {
    const tests = [_]struct {
        l: usize,
        r: usize,
        op: []const u8,
        input: []const u8,
    }{
        .{ .input = "10 + 5", .l = 10, .r = 5, .op = "+" },
        .{ .input = "10 - 5", .l = 10, .r = 5, .op = "-" },
        .{ .input = "10 * 5", .l = 10, .r = 5, .op = "*" },
        .{ .input = "10 / 5", .l = 10, .r = 5, .op = "/" },
        .{ .input = "10 > 5", .l = 10, .r = 5, .op = ">" },
        .{ .input = "10 < 5", .l = 10, .r = 5, .op = "<" },
        .{ .input = "10 == 5", .l = 10, .r = 5, .op = "==" },
        .{ .input = "10 != 5", .l = 10, .r = 5, .op = "!=" },
    };

    for (tests) |x| {
        var lexer = Lexer.init(x.input);
        var parser = Parser.new(talloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        if (program.statements.items.len != 1) {
            std.log.err("len: {d}", .{program.statements.items.len});
            return error.NotEnoughStatements;
        }

        const stmt = program.statements.items[0].exp_statement;

        const exp = stmt.expression.infix;

        const operator = exp.operator;

        if (!std.mem.eql(u8, @tagName(operator), x.op))
            return error.UnexpectedOP;

        const left = exp.left.integer;
        const right = exp.right.integer;

        if (left.value != x.l or right.value != x.r)
            return error.UnexpectedValue;
    }
}

test "Parse Prefix OP (!)" {
    const input = "!5;";
    const output = 5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.prefix;

    if (.@"!" != exp.operator) {
        return error.UnexpecedOperator;
    }

    const integer = exp.right.integer;

    if (output != integer.value) {
        return error.UnexpectedValue;
    }
}

test "Comment (bruh)" {
    const input =
        \\#loli gos brrr
        \\-5
    ;

    const output = -5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.prefix;

    if (.@"-" != exp.operator) {
        return error.UnexpecedOperator;
    }

    const integer = exp.right.integer;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }
}

test "parse Prefix OP (-)" {
    const input = "-5;";
    const output = -5;

    var lexer = Lexer.init(input);
    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0].exp_statement;

    const exp = stmt.expression.prefix;

    if (.@"-" != exp.operator) {
        return error.UnexpecedOperator;
    }

    const integer = exp.right.integer;

    if (output != -integer.value) {
        return error.UnexpectedValue;
    }
}

test "Eval Integer Literal Expression" {
    const input = "5;";

    var lexer = Lexer.init(input);

    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0];

    if (stmt != .exp_statement) {
        return error.ExprectAnExpression;
    }

    const literal = stmt.exp_statement.expression.integer;

    if (5 != literal.value) {
        return error.UnexpectedValue;
    }

    // if (!std.mem.eql(u8, "5", literal.tokenLiteral())) {
    //     return error.UnexpectedValue;
    // }
}

test "eval Expression" {
    const input = "return foobar;";

    var lexer = Lexer.init(input);

    var parser = Parser.new(talloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    if (program.statements.items.len != 1) {
        std.log.err("len: {d}", .{program.statements.items.len});
        return error.NotEnoughStatements;
    }

    const stmt = program.statements.items[0];

    const ret = stmt.@"return";

    const ident = ret.value.identifier;

    const value = ident.value;

    if (!std.mem.eql(u8, "foobar", value)) {
        return error.UnexpectedValue;
    }

    // if (!std.mem.eql(u8, "foobar", ident.tokenLiteral())) {
    //     return error.UnexpectedValue;
    // }
}

// test "eval Program" {
//     const stmts = std.ArrayList(ast.Statement).init(talloc);
//     defer stmts.deinit();
//
//     const stmt = ast.Statement{
//         .@"var" = .{
//             .token = .{
//                 .type = .con,
//                 .literal = "con",
//             },
//             .name = .{
//                 .token = .{
//                     .type = .identifier,
//                     .literal = "myVar",
//                 },
//                 .value = "myVar",
//             },
//             .value = .{
//                 .identifier = .{
//                     .token = .{
//                         .type = .identifier,
//                         .literal = "anotherVar",
//                     },
//                     .value = "anotherVar",
//                 },
//             },
//         },
//     };
//
//     try stmts.append(stmt);
//
//     const program = ast.Program{ .statements = stmts };
//     _ = program;
//     // std.debug.print("{s}", .{program.tokenLiteral()});
//     // try std.testing.expect( std.mem.eql(u8, "con myVar = anotherVar;", program.tokenLiteral()) );
// }

test "Parse RETURN statements: Size" {
    const expected_value = struct { i64, bool, []const u8 }{ 5, true, "y" };

    const tests = [_]struct {
        input: []const u8,
    }{
        .{ .input = "return 5;" },
        .{ .input = "return true;" },
        .{ .input = "return y;" },
    };

    inline for (tests, expected_value) |x, k| {
        var lexer = Lexer.init(x.input);

        var p = Parser.new(talloc, &lexer);
        defer p.deinit();

        const program = try p.parseProgram();

        try std.testing.expect(program.statements.items.len == 1);
        const stmt = program.statements.items[0];

        const val = stmt.@"return".value;

        try testLiteralExpression(val, k);
    }
}

test "Parse NULL statements" {
    const tests = [_]struct {
        input: []const u8,
    }{
        .{ .input = "return null;" },
        .{ .input = "return;" },
        .{ .input = "return" },
    };

    inline for (tests) |x| {
        var lexer = Lexer.init(x.input);

        var p = Parser.new(talloc, &lexer);
        defer p.deinit();

        const program = try p.parseProgram();

        try std.testing.expect(program.statements.items.len == 1);
        const stmt = program.statements.items[0];

        const val = stmt.@"return".value;

        try std.testing.expect(val.* == .null);
        // try testLiteralExpression(val, k);
    }
}

test "Parse con statements" {
    const expected_value = struct { i64, bool, []const u8 }{ 5, true, "y" };

    const tests = [_]struct {
        input: []const u8,
        expected_indetifier: []const u8,
    }{
        .{ .input = "con x = 5;", .expected_indetifier = "x" },
        .{ .input = "con x = true;", .expected_indetifier = "x" },
        .{ .input = "con x = y;", .expected_indetifier = "x" },
    };

    inline for (tests, expected_value) |x, k| {
        var lexer = Lexer.init(x.input);

        var p = Parser.new(talloc, &lexer);
        defer p.deinit();

        const program = try p.parseProgram();

        try std.testing.expect(program.statements.items.len == 1);
        const stmt = program.statements.items[0];

        const val = stmt.con.value;

        try testLiteralExpression(val, k);
    }
}

test "Token test" {
    const input =
        \\con x = 100 + if (!true) 5 else -10;
        \\-5;
        \\"ola mundo";
        \\+=
    ;

    const tokens = [_]Type{ .con, .identifier, .@"=", .integer, .@"+", .@"if", .@"(", .@"!", .true, .@")", .integer, .@"else", .@"-", .integer, .@";", .@"-", .integer, .@";", .string, .@";", .@"+=" };

    var lexer = Lexer.init(input);
    var tok = lexer.nextToken();

    var i: usize = 0;
    while (tok.type != .eof) {
        try std.testing.expect(tok.type == tokens[i]);
        tok = lexer.nextToken();
        i += 1;
    }
}
