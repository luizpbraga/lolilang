const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const Error = @import("Error.zig");
const stderr = std.io.getStdErr();
const ast = @import("ast.zig");

pub var emitbytecode = false;

pub fn format(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();
    _ = node;

    if (parser.errors.counter != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var fmt = std.ArrayList(u8).init(allocator);
    defer fmt.deinit();
}

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    var err: Error = .{ .input = input, .msg = .init(allocator) };
    defer err.deinit();

    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    // std.debug.print("defined types: {s}", .{Parser.types});

    if (parser.errors.counter != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var compiler: Compiler = try .init(allocator, &err);
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
        std.log.info("token position:\n{d}", .{code.positions});
        std.log.info("bytecode instructions:\n{s}", .{fmt});
    }

    var vm: Vm = try .init(allocator, &code, &err);
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
