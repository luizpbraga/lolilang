test "function - return concat string " {
    var lexer = Lexer.init(
        \\var f = func(){ "ola" + "mundo" };
        \\f();
    );
    var parser = try Parser.new(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parseProgram();
    var env = Environment.init(allocator);
    defer env.deinit();
    const obj = try env.eval(.{ .statement = .{ .program_statement = program } });
    try std.testing.expect(std.mem.eql(u8, obj.string.value, "olamundo"));
}
