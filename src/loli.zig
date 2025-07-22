const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const Error = @import("Error.zig");

pub var emitbytecode = false;

pub fn runVm(allocator: std.mem.Allocator, input: []const u8, err: *Error) !void {
    const stderr = std.fs.File.stderr();
    var buff: [100]u8 = undefined;
    var w = stderr.writer(&buff);

    var lexer: Lexer = .init(input);
    var parser: Parser = .init(allocator, &lexer, err);
    defer parser.deinit();

    const node = try parser.parse();

    if (parser.errors.msg.items.len != 0) {
        try w.interface.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var compiler: Compiler = try .init(allocator, err);
    defer compiler.deinit();

    compiler.compile(node) catch |comp_err|
        return switch (comp_err) {
            error.Compilation => try w.interface.writeAll(
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
        std.log.info("token postion:\n{any}", .{code.positions});
        std.log.info("bytecode instructions:\n{s}", .{fmt});
    }

    var vm: Vm = try .init(allocator, &code, err);
    defer vm.deinit();

    vm.run() catch |vm_err| {
        try @import("memory.zig").collectGarbage(&vm);
        return switch (vm_err) {
            error.Runtime => try w.interface.writeAll(
                vm.errors.msg.items,
            ),
            else => vm_err,
        };
    };
}
