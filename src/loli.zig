const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Vm = @import("Vm.zig");
const Error = @import("Error.zig");
const gc = @import("gc.zig");

pub var emitbytecode = false;

pub fn runVm(gpa: std.mem.Allocator, input: []const u8) !void {
    var stderr: std.fs.File = .stderr();
    defer stderr.close();

    var errors: Error = .{};
    var lexer: Lexer = .init(input);
    var parser: Parser = .init(&lexer, &errors);

    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const node = try parser.parse(arena);
    if (!errors.check()) {
        return try errors.writeError(stderr);
    }

    var compiler: Compiler = try .init(gpa, &errors);
    defer compiler.deinit(gpa);

    compiler.compile(gpa, node) catch |comp_err|
        return switch (comp_err) {
            error.Compilation => try errors.writeError(stderr),
            else => comp_err,
        };

    var code = try compiler.bytecode(gpa);
    defer code.deinit(gpa);

    if (emitbytecode) {
        const fmt = try @import("code.zig").formatInstruction(gpa, code.instructions);
        defer gpa.free(fmt);
        std.log.info("bytecode instructions:\n{s}", .{fmt});
    }

    var vm: Vm = try .init(gpa, &code, &errors);
    defer vm.deinit(gpa);

    vm.run(gpa) catch |vm_err| {
        try gc.collectGarbage(gpa, &vm);
        return switch (vm_err) {
            error.Runtime => try errors.writeError(stderr),
            else => vm_err,
        };
    };
}
