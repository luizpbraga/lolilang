const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const Error = @import("Error.zig");
const stderr = std.io.getStdErr();

pub var emitbytecode = false;

pub fn runVm(allocator: std.mem.Allocator, input: []const u8, err: *Error) !void {
    // var err: Error = .{ .input = input, .msg = .init(allocator) };
    // defer err.deinit();

    var lexer: Lexer = .init(input);
    var parser: Parser = .init(allocator, &lexer, err);
    defer parser.deinit();

    const node = try parser.parse();
    // defer node.deinit(allocator);

    if (parser.errors.msg.items.len != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var compiler: Compiler = try .init(allocator, err);
    defer compiler.deinit();

    compiler.compile(node) catch |comp_err|
        return switch (comp_err) {
        error.Compilation => try stderr.writeAll(
            compiler.errors.msg.items,
        ),
        else => comp_err,
    };

    // // assert the bytecodes
    var code = try compiler.bytecode();
    defer code.deinit(&compiler);

    if (emitbytecode) {
        const fmt = try @import("code.zig").formatInstruction(allocator, code.instructions);
        defer allocator.free(fmt);
        std.log.info("token postion:\n{d}", .{code.positions});
        std.log.info("bytecode instructions:\n{s}", .{fmt});
    }

    var vm: Vm = try .init(allocator, &code, err);
    defer vm.deinit();

    vm.run() catch |vm_err| {
        try @import("memory.zig").collectGarbage(&vm);
        return switch (vm_err) {
            error.Runtime => try stderr.writeAll(
                vm.errors.msg.items,
            ),
            else => vm_err,
        };
    };
}
