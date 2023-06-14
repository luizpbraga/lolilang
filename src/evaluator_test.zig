const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;
const eval = @import("evaluator.zig").eval;
const allocator = std.testing.allocator;

test "for loop range" {
    var lexer = Lexer.init(
        \\var list = {10,20,30,40}
        \\for i, j in list { 
        \\  print(i,j)
        \\}
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    _ = obj;
}

test "for loop" {
    var lexer = Lexer.init(
        \\var i = 0
        \\for i < 10 { 
        \\  var i = i + 1
        \\}
        \\print(i)
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    _ = obj;
}

// test "hash map" {
//     var lexer = Lexer.init(
//         \\const y = "!"
//         \\var map = { "2": "data"+y, y : y }
//         \\var x = map["2"]
//     );
//     var parser = try Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram(allocator);
//     defer program.statements.deinit();

//     var env = object.Environment.init(allocator);
//     defer env.deinit();

//     var obj = try eval(
//         allocator,
//         .{
//             .statement = .{
//                 .program_statement = program,
//             },
//         },
//         &env,
//     );

//     try std.testing.expect(obj == .string);
// }

test "print" {
    var lexer = Lexer.init(
        \\var foo = {1,2,3}
        \\print(foo)
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj == .null);
}

test "string length 1" {
    var lexer = Lexer.init(
        \\var str = "1" + "0"
        \\const y = 1 + str.len + 1
        \\return y
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    try std.testing.expect(obj.integer.value == 4);
}

test "string length 2" {
    var lexer = Lexer.init(
        \\var str_a = "amor" + "amor"
        \\var str_b = "amor"
        \\return fn(x,y){ return x + y }(str_a.len, str_b.len)
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    try std.testing.expect(obj.integer.value == 12);
}

test "redefine (=)" {
    var lexer = Lexer.init(
        \\var foo = 20
        \\foo = if (true) {
        \\      var y = 0
        \\      y = 1
        \\      foo + y
        \\} else { 0 }
        \\return foo
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 21);
}
test "redefine returns?" {
    var lexer = Lexer.init(
        \\var foo = 20
        \\foo = 21
        \\return foo
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 21);
}

test "redefine 2: arrays" {
    var lexer = Lexer.init(
        \\var foo = {1,2,3}
        \\foo[ if (true) { 1+0 } else {0} ] = 69
        \\return foo[1]
    );

    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    try std.testing.expect(obj.integer.value == 69);

    // for (obj.array.elements, [3]i64{ 1, 69, 3 }) |int, val| {
    //     try std.testing.expect(int.integer.value == val);
    // }
}

test "redefine 3: arrays += " {
    const inputs = [_][]const u8{
        \\var foo = {0,1,2}; foo[0] += 10; return foo[0]
        ,
        \\var foo = {0,20,2}; foo[1] -= 10; return foo[1]
        ,
        \\var foo = {0,"1", 1}; foo[2] *= 10; return foo[2]
        ,
        \\var foo = {0,"1", 2, 100}; foo[3] /= 10; return foo[3]
        ,
    };

    for (inputs) |input| {
        var lexer = Lexer.init(input);

        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var env = object.Environment.init(allocator);
        defer env.deinit();

        var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
        try std.testing.expect(obj.integer.value == 10);
    }
}

test "string concat" {
    const inputs = [_][]const u8{
        \\ return "1" + "69"
        ,
        \\var foo = {0,1,2}; foo[ 1 ] += "69"; return foo[1]
        ,
        \\var foo = {0,"1",2}; foo[ 1 ] += 69; return foo[1]
        ,
        \\var foo = {0,"1",2}; foo[ 1 ] += "69"; return foo[1]
        ,
    };

    for (inputs) |input| {
        var lexer = Lexer.init(input);

        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var env = object.Environment.init(allocator);
        defer env.deinit();

        var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
        try std.testing.expect(std.mem.eql(u8, obj.string.value, "169"));
    }
}

test "code example" {
    var lexer = Lexer.init(
        \\var f = fn(x, y, z){
        \\    var p = if x > 0 {3} else {0}
        \\    var t = fn(g){ return if true {g} else {0} }(p)
        \\    return x + y + z + t
        \\}
        \\var x = f(1,2,3)
        \\var y = {x, 0, x, 0}
        \\y[0]
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 9);
}

test "Array Literal" {
    var lexer = Lexer.init("{1, 2, 3}");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    var array = obj.array;
    try std.testing.expect(array.elements.len == 3);

    for (array.elements, [_]i64{ 1, 2, 3 }) |a, b| {
        try std.testing.expect(a.integer.value == b);
    }
}

test "Array Index" {
    var lexer = Lexer.init("{1, 2, 3}[0]");
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    var integer = obj.integer;

    try std.testing.expect(integer.value == 1);
}

// test "Array Index 2" {
//     var lexer = Lexer.init(
//         \\var array = {1, 2, 3};
//         \\var idx = 0;
//         \\array[idx] == 1;
//     );
//     var parser = try Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram(allocator);
//     defer program.statements.deinit();

//     var env = object.Environment.init(allocator);
//     defer env.deinit();

//     var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

//     var boole = obj.boolean;

//     try std.testing.expect(boole.value == true);
// }

test "const statement " {
    var lexer = Lexer.init(
        \\const x = "olamundo"
        \\var y = 10
        \\y = 1
        \\return y + x
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(std.mem.eql(u8, obj.string.value, "1olamundo"));
}

test "var/const block  " {
    var lexer = Lexer.init(
        \\const { x = "olamundo" y = 1}
        \\var { z = "olamundo" h = 1 }
        \\return h + x
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(std.mem.eql(u8, obj.string.value, "1olamundo"));
}

// test "function - return concat string " {
//     var lexer = Lexer.init(
//         \\var f = fn(){ "ola" + "mundo" };
//         \\f();
//     );
//     var parser = try Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram(allocator);
//     defer program.statements.deinit();

//     var env = object.Environment.init(allocator);
//     defer env.deinit();

//     var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

//     try std.testing.expect(std.mem.eql(u8, obj.string.value, "olamundo"));
// }

test "function call " {
    var lexer = Lexer.init(
        \\var f = fn(){ return -5 };
        \\f();
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == -5);
}

test "function Obj 0" {
    const input = "fn(x, y){ return -10 + x + y; }(1,2); ";

    var lexer = Lexer.init(input);
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == -10 + 1 + 2);
}

test "function Obj 1" {
    const input = "var g = fn(x, y){ return -10 + x + y; }; g(1,2); ";

    var lexer = Lexer.init(input);
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == -10 + 1 + 2);
}

test "string" {
    var lexer = Lexer.init(
        \\"-69 - 1" + "ola"
    );
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

    var obj = try eval(allocator, node, &env);

    try std.testing.expect(std.mem.eql(u8, obj.string.value, "-69 - 1ola"));
}

test "int" {
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

    var obj = try eval(allocator, node, &env);

    try std.testing.expect(obj.integer.value == -69 - 1);
}

test "bool" {
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
    var obj = try eval(allocator, node, &env);

    try std.testing.expect(obj.boolean.value == (!!true == true));
}

test "infix int" {
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
        var obj = try eval(allocator, node, &env);

        try std.testing.expect(obj.integer.value == x.value);
    }
}
test "infix bool" {
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
        var obj = try eval(allocator, node, &env);
        try std.testing.expect(obj.boolean.value == x.value);
    }
}

test "if-else expression" {
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
        var obj = try eval(allocator, node, &env);

        switch (obj) {
            .integer => |int| try std.testing.expect(int.value == x.value),
            .null => |nil| try std.testing.expect(nil.value == x.value),
            else => return error.UnexpectedObj,
        }
    }
}

test "return" {
    var lexer = Lexer.init(
        \\if (10 > 1) { if (10 > 1) { return -10; } return 1; }
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
    var obj = try eval(allocator, node, &env);

    var int = obj.@"return".value.integer;

    try std.testing.expect(int.value == -10);
}

test "env" {
    var lexer = Lexer.init(
        \\var x = 10;
        \\var y = 2 * x + if (true) {10} else {-1};
        \\y;
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram(allocator);
    defer program.statements.deinit();

    var env = object.Environment.init(allocator);
    defer env.deinit();

    var obj = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);

    try std.testing.expect(obj.integer.value == 30);
}
