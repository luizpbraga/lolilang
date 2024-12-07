const Vm = @This();

const STACK_SIZE = 2048;

alloc: std.mem.Allocator,
stack: std.BoundedArray(object.Object, STACK_SIZE),
bcode: *Compiler.Bytecode,
// points to the next value. top is stack[sp - 1]
sp: usize = 0,

fn init(alloc: anytype, b: *Compiler.Bytecode) !Vm {
    return .{ .alloc = alloc, .bcode = b, .stack = try .init(0) };
}

fn deinit(vm: *Vm) void {
    _ = vm;
}

fn run(vm: *Vm) !void {
    // decode cycle
    var ip: usize = 0;
    while (ip < vm.bcode.instructions.len) : (ip += 1) {
        const op: code.Opcode = @enumFromInt(vm.bcode.instructions[ip]);
        switch (op) {
            .constant => {
                const const_index = std.mem.readInt(u16, vm.bcode.instructions[ip + 1 ..][0..2], .big);
                ip += 2;
                try vm.push(vm.bcode.constants[const_index]);
            },
        }
    }
}

fn push(vm: *Vm, obj: object.Object) !void {
    if (vm.sp >= STACK_SIZE) return error.VMStackOverflow;
    try vm.stack.insert(vm.sp, obj);
    vm.sp += 1;
}

fn stackTop(vm: *Vm) ?object.Object {
    if (vm.sp == 0) return null;
    return vm.stack.get(vm.sp - 1);
}

test "Integer Arithmetic" {
    const tests: []const struct {
        input: []const u8,
        expected: usize,
    } = &.{
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 2", .expected = 2 }, // FIX
    };

    for (tests) |t| {
        var lexer: Lexer = .init(t.input);
        var parser: Parser = .new(talloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var compiler: Compiler = .init(talloc);
        defer compiler.deinit();

        try compiler.compile(.{ .statement = .{ .program_statement = program } });
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        var vm: Vm = try .init(talloc, &b);
        defer vm.deinit();

        try vm.run();

        const obj = vm.stackTop() orelse {
            std.log.warn("Empty Stack", .{});
            continue;
        };

        try expectedObject(t.expected, obj);
    }
}

fn checkIntegerObject(exp: i64, act: object.Object) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAInteger,
    };

    if (result.value != exp) return error.WrongIntegerValue;
}

fn expectedObject(expected: anytype, actual: object.Object) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int => try checkIntegerObject(@intCast(expected), actual),
        else => {},
    }
}

const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const talloc = std.testing.allocator;
