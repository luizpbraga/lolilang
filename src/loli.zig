const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");

pub var emitbytecode = false;

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    var compiler: Compiler = try .init(allocator);
    defer compiler.deinit();

    try compiler.compile(node);

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

    vm.run() catch |err| {
        try @import("memory.zig").collectGarbage(&vm);
        return err;
    };
}
