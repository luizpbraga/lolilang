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

    var compiler: Compiler = .init(allocator);
    defer compiler.deinit();

    var vm: Vm = undefined;
    vm.sp = 0;

    var buffer: [500]u8 = undefined;
    try stdout.writeAll(">>> ");
    while (try stdin.reader().readUntilDelimiterOrEof(&buffer, '\n')) |input| {
        if (std.mem.eql(u8, input, "\\lc")) {
            for (compiler.constants.items) |con| try writer.print("\t{}\n", .{con});
            try writer.writeAll("\n>>> ");
            continue;
        }

        if (std.mem.eql(u8, input, "\\lg")) {
            for (compiler.symbols.store.keys()) |con| try writer.print("\t{s}\n", .{con});
            try writer.writeAll("\n>>> ");
            continue;
        }

        var lexer = Lexer.init(input);

        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const node = parser.parse() catch |err| {
            std.log.err("parser error: {s}\n>>>", .{@errorName(err)});
            continue;
        };

        compiler.compile(node) catch |err| {
            try writer.print("compiler error: {s}\n>>> ", .{@errorName(err)});
            continue;
        };

        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        vm.bcode = &b;

        try vm.run();

        const obj = vm.lastPopped();

        vm.sp = 0;

        switch (obj) {
            inline .float, .integer, .boolean => |boo| try writer.print("{}\n>>> ", .{boo.value}),

            .string => |str| try writer.print("{s}\n>>> ", .{str.value}),

            else => {
                try writer.print("{}\n>>> ", .{obj});
            },
        }

        compiler.last_ins = undefined;
        compiler.prev_ins = undefined;
        // for (compiler.instructions.items) |ins| compiler.allocator.free(ins);
    }
}

pub fn runInterpreter(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    var env = Environment.init(allocator);
    defer env.deinit();

    _ = try env.eval(node);
}

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    var compiler: Compiler = .init(allocator);
    defer compiler.deinit();

    try compiler.compile(node);

    // // assert the bytecodes
    var code = try compiler.bytecode();
    defer code.deinit(&compiler);

    var vm: Vm = .init(&code);
    try vm.run();
}
