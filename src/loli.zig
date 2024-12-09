const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Environment = @import("Environment.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");

pub fn runInterpreter(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = parser.parseProgram() catch |err| {
        std.log.err("{s}", .{@errorName(err)});
        return;
    };

    var env = Environment.init(allocator);
    defer env.deinit();

    _ = try env.eval(.{ .statement = .{ .program_statement = program } });
}

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const program = parser.parseProgram() catch |err| {
        std.log.err("{s}", .{@errorName(err)});
        return;
    };

    var compiler: Compiler = .init(allocator);
    defer compiler.deinit();

    try compiler.compile(.{ .statement = .{ .program_statement = program } });
    // // assert the bytecodes
    var b = try compiler.bytecode();
    defer b.deinit(&compiler);

    var vm: Vm = .init(&b);

    try vm.run();
}
