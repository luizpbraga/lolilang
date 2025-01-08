const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");

pub var emitbytecode = false;

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    const stderr = std.io.getStdErr();

    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    if (parser.errors.counter != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var compiler: Compiler = try .init(allocator);
    defer compiler.deinit();

    compiler.compile(node) catch |err| switch (err) {
        error.Compilation => {
            return try stderr.writeAll(
                compiler.errors.msg.items,
            );
        },
        else => return err,
    };

    // // assert the bytecodes
    var code = try compiler.bytecode();
    defer code.deinit(&compiler);

    if (emitbytecode) {
        const fmt = try @import("code.zig").formatInstruction(allocator, code.instructions);
        defer allocator.free(fmt);
        std.log.info("bytecode instructions:\n{s}", .{fmt});
    }

    var vm: Vm = try .init(allocator, &code);
    defer vm.deinit();

    vm.run() catch |err| switch (err) {
        error.Runtime => {
            return try stderr.writeAll(
                vm.errors.msg.items,
            );
        },
        else => {
            try @import("memory.zig").collectGarbage(&vm);
            return err;
        },
    };
}
