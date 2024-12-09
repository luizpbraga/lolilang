const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Environment = @import("Environment.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");

pub fn startRepl(allocator: anytype) !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();
    var writer = stdout.writer();

    var buffer: [500]u8 = undefined;
    try stdout.writeAll(">>> ");
    while (try stdin.reader().readUntilDelimiterOrEof(&buffer, '\n')) |input| {
        var lexer = Lexer.init(input);

        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = parser.parseProgram() catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            return;
        };

        var compiler: Compiler = .init(allocator);
        defer compiler.deinit();

        compiler.compile(.{ .statement = .{ .program_statement = program } }) catch |err| {
            try writer.print("error: {s}\n>>> ", .{@errorName(err)});
            continue;
        };
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        var vm: Vm = .init(&b);
        try vm.run();
        const obj = vm.lastPopped();

        switch (obj) {
            inline .float, .integer, .boolean => |boo| try writer.print("code: {x}\n{}\n>>> ", .{ vm.bcode.instructions, boo.value }),

            .string => |str| try writer.print("code: {x}\n{s}\n>>> ", .{ vm.bcode.instructions, str.value }),

            else => {
                try writer.print("code: {x}\n{}\n>>> ", .{ vm.bcode.instructions, obj });
            },
        }
    }
}

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
