const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TokenType = @import("Token.zig").TokenType;
const Environment = @import("Environment.zig");
const allocator = std.testing.allocator;

// test "switch " {
//     var lexer = Lexer.init(
//         \\con x = 30
//         \\con t = x + switch 0 {
//         \\  0    => { 0 },
//         \\  1    => { 1 },
//         \\  3    => { 3 },
//         \\  else => {-1 }
//         \\}
//         \\print( t )
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//     _ = obj;
// }
//
// test "for loop range" {
//     var lexer = Lexer.init(
//         \\//for loop test
//         \\con list = {10,20,30,40}
//         \\for i, j in list {
//         \\  print(i,j, list)
//         \\}
//         \\print( "ooooooooooh ma goooood" )
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//     _ = obj;
// }
//
// test "for loop" {
//     var lexer = Lexer.init(
//         \\con i = 0
//         \\for i < 10 {
//         \\  const i = i + 1
//         \\}
//         \\print(i)
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//     _ = obj;
// }
//
// test "hash map" {
//     var lexer = Lexer.init(
//         \\con y = "!"
//         \\con map = { "2": "data"+y, y : y }
//         \\con x = map["2"]
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(
//         allocator,
//         .{
//             .statement = .{
//                 .program_statement = program,
//             },
//         },
//         &env,
//     );
//
//     try std.testing.expect(obj == .string);
// }
//
test "print" {
    var lexer = Lexer.init(
        \\con foo = [1,2,3]
        \\print(foo)
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj == .null);
}

test "string length 1" {
    var lexer = Lexer.init(
        \\con str = "1" + "0"
        \\con y = 1 + str.len + 1
        \\return y
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
    try std.testing.expect(obj.integer.value == 4);
}

test "string length 2" {
    var lexer = Lexer.init(
        \\con str_a = "amor" + "amor"
        \\con str_b = "amor"
        \\return fn(x,y){ return x + y }(str_a.len, str_b.len)
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
    try std.testing.expect(obj.integer.value == 12);
}

// test "redefine (=)" {
//     var lexer = Lexer.init(
//         \\var foo = 20
//         \\foo = if (true) {
//         \\      var y = 0
//         \\      y = 1
//         \\      foo + y
//         \\} else { 0 }
//         \\return foo
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//
//     try std.testing.expect(obj.integer.value == 21);
// }
//
test "redefine and return" {
    var lexer = Lexer.init(
        \\var foo = 20
        \\foo = 21
        \\return foo
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == 21);
}

test "redefine 2: arrays" {
    var lexer = Lexer.init(
        \\var foo =[1,2,3]
        \\foo[ if (true) { 1+0 } else {0} ] = 69
        \\return foo[1]
    );

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
    try std.testing.expect(obj.integer.value == 69);

    // for (obj.array.elements, [3]i64{ 1, 69, 3 }) |int, val| {
    //     try std.testing.expect(int.integer.value == val);
    // }
}

test "redefine 3: arrays += " {
    const inputs = [_][]const u8{
        \\var foo = [0,1,2]; foo[0] += 10; return foo[0]
        ,
        \\var foo = [0,20,2]; foo[1] -= 10; return foo[1]
        ,
        \\var foo = [0,"1", 1]; foo[2] *= 10; return foo[2]
        ,
        \\var foo = [0,"1", 2, 100]; foo[3] /= 10; return foo[3]
        ,
    };

    for (inputs) |input| {
        var lexer = Lexer.init(input);

        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var env = Environment.init(allocator);
        defer env.deinit();

        const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
        try std.testing.expect(obj.integer.value == 10);
    }
}

test "string concat" {
    const inputs = [_][]const u8{
        // \\return "1" + "69"
        // ,
        // \\var foo = [0,1,2]; foo[ 1 ] += "69"; return foo[1]
        // ,
        // \\var foo = [0,"1",2]; foo[ 1 ] += 69; return foo[1]
        // ,
        // \\var foo = [0,"1",2]; foo[ 1 ] += "69"; return foo[1]
        // ,
    };

    for (inputs) |input| {
        var lexer = Lexer.init(input);

        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var env = Environment.init(allocator);
        defer env.deinit();

        const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
        try std.testing.expect(std.mem.eql(u8, obj.string.value, "169"));
    }
}

test "code example" {
    var lexer = Lexer.init(
        \\con f = fn(x, y, z){
        \\    con p = if x > 0 {3} else {0}
        \\    con t = fn(g){ return if true {g} else {0} }(p)
        \\    return x + y + z + t
        \\}
        \\con x = f(1,2,3)
        \\con y = [x, 0, x, 0]
        \\y[0]
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == 9);
}

test "Array Literal" {
    var lexer = Lexer.init("[1, 2, 3]");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    const array = obj.array;
    try std.testing.expect(array.elements.len == 3);

    for (array.elements, [_]i64{ 1, 2, 3 }) |a, b| {
        try std.testing.expect(a.integer.value == b);
    }
}

test "Array Index" {
    var lexer = Lexer.init("[1, 2, 3][0]");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    const integer = obj.integer;

    try std.testing.expect(integer.value == 1);
}

// test "Array Index 2" {
//     var lexer = Lexer.init(
//         \\con array = {1, 2, 3};
//         \\con idx = 0;
//         \\array[idx] == 1;
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram();

//     var env = Environment.init(allocator);
//     defer env.deinit();

//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

//     const boole = obj.boolean;

//     try std.testing.expect(boole.value == true);
// }

// test "con statement " {
//     var lexer = Lexer.init(
//         \\con x = "olamundo"
//         \\var y = 10
//         \\y = 1
//         \\return y + x
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//
//     try std.testing.expect(std.mem.eql(u8, obj.string.value, "1olamundo"));
// }

// test "const/const block  " {
//     var lexer = Lexer.init(
//         \\con { x = "olamundo" y = 1}
//         \\con { z = "olamundo" h = 1 }
//         \\return h + x
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram();
//
//     var env = Environment.init(allocator);
//     defer env.deinit();
//
//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
//
//     try std.testing.expect(std.mem.eql(u8, obj.string.value, "1olamundo"));
// }

// test "function - return concat string " {
//     var lexer = Lexer.init(
//         \\con f = fn(){ "ola" + "mundo" };
//         \\f();
//     );
//     var parser = Parser.new(allocator, &lexer);
//     defer parser.deinit();

//     const program = try parser.parseProgram();

//     var env = Environment.init(allocator);
//     defer env.deinit();

//     const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

//     try std.testing.expect(std.mem.eql(u8, obj.string.value, "olamundo"));
// }

test "function call " {
    var lexer = Lexer.init(
        \\con f = fn(){ return -5 };
        \\f();
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == -5);
}

test "function Obj 0" {
    const input = "fn(x, y){ return -10 + x + y; }(1,2); ";

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == -10 + 1 + 2);
}

test "function Obj 1" {
    const input = "con g = fn(x, y){ return -10 + x + y; }; g(1,2); ";

    var lexer = Lexer.init(input);
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == -10 + 1 + 2);
}

test "string" {
    var lexer = Lexer.init(
        \\"-69 - 1" + "ola"
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    const node = ast.Node{ .expression = exp };

    const obj = try env.eval(node);

    try std.testing.expect(std.mem.eql(u8, obj.string.value, "-69 - 1ola"));
}

test "int" {
    var lexer = Lexer.init("-69 - 1");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();

    const stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    const node = ast.Node{ .expression = exp };

    const obj = try env.eval(node);

    try std.testing.expect(obj.integer.value == -69 - 1);
}

test "bool" {
    var lexer = Lexer.init("!!true == true");
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    const stmt = program.statements.items[0];

    // const literal = stmt.expression_statement.expression.integer_literal;
    // _ = literal;
    const exp = stmt.expression_statement.expression;

    const node = ast.Node{ .expression = exp };

    var env = Environment.init(allocator);
    defer env.deinit();
    const obj = try env.eval(node);

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
        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        const stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        const node = ast.Node{ .expression = exp };

        var env = Environment.init(allocator);
        defer env.deinit();
        const obj = try env.eval(node);

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
        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        const stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        const node = ast.Node{ .expression = exp };

        var env = Environment.init(allocator);
        defer env.deinit();
        const obj = try env.eval(node);
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
        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        const stmt = program.statements.items[0];

        const exp = stmt.expression_statement.expression;

        const node = ast.Node{ .expression = exp };

        var env = Environment.init(allocator);
        defer env.deinit();
        const obj = try env.eval(node);

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
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    const stmt = program.statements.items[0];

    const exp = stmt.expression_statement.expression;

    const node = ast.Node{ .expression = exp };

    var env = Environment.init(allocator);
    defer env.deinit();
    const obj = try env.eval(node);

    const int = obj.@"return".value.integer;

    try std.testing.expect(int.value == -10);
}

test "env" {
    var lexer = Lexer.init(
        \\con x = 10;
        \\con y = 2 * x + if (true) {10} else {-1};
        \\y;
    );
    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    var env = Environment.init(allocator);
    defer env.deinit();
    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });

    try std.testing.expect(obj.integer.value == 30);
}
