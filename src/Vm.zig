const Vm = @This();

const STACK_SIZE = 2048;

stack: [STACK_SIZE]object.Object,
bcode: *Compiler.Bytecode,
// points to the next value. top is stack[sp - 1]
sp: usize = 0,

pub fn init(b: *Compiler.Bytecode) Vm {
    return .{ .bcode = b, .stack = undefined };
}

pub fn run(vm: *Vm) !void {
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

            .add, .sub, .mul, .div => try vm.executeBinaryOperation(op),

            .eq, .neq, .gt => try vm.executeComparison(op),

            .not, .min => try vm.executePrefixOperation(op),

            .true, .false => try vm.push(.{ .boolean = .{ .value = if (op == .true) true else false } }),

            .pop => _ = vm.pop(),
        }
    }
}

fn executePrefixOperation(vm: *Vm, op: code.Opcode) !void {
    const obj = vm.pop();

    const otype = obj.objType();

    switch (otype) {
        .integer => {
            if (op == .min) {
                return try vm.push(.{ .integer = .{ .value = -obj.integer.value } });
            }
            return error.UnknowIntegerOperation;
        },
        .boolean => {
            if (op == .not) {
                return try vm.push(.{ .boolean = .{ .value = !obj.boolean.value } });
            }
            return error.UnknowBooleanOperation;
        },

        else => return error.UnsupportedOperation,
    }
}

fn executeBinaryOperation(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    const rtype = right.objType();
    const ltype = left.objType();

    if (rtype == .integer and ltype == .integer) {
        const right_val = right.integer.value;
        const left_val = left.integer.value;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            else => return error.UnknowIntegerOperation,
        };
        return try vm.push(.{ .integer = .{ .value = result } });
    }

    return error.UnsupportedOperation;
}

fn executeComparison(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    const rtype = right.objType();
    const ltype = left.objType();

    if (rtype == .boolean and ltype == .boolean) {
        const right_val = right.boolean.value;
        const left_val = left.boolean.value;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = .{ .value = result } });
    }

    if (rtype == .integer and ltype == .integer) {
        const right_val = right.integer.value;
        const left_val = left.integer.value;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            else => return error.UnknowIntegerOperation,
        };
        return try vm.push(.{ .boolean = .{ .value = result } });
    }

    return error.UnsupportedOperation;
}

fn pop(vm: *Vm) object.Object {
    const o = vm.stack[vm.sp - 1];
    vm.sp -= 1;
    return o;
}

fn push(vm: *Vm, obj: object.Object) !void {
    if (vm.sp >= STACK_SIZE) return error.VMStackOverflow;
    vm.stack[vm.sp] = obj;
    vm.sp += 1;
}

fn top(vm: *Vm) ?object.Object {
    if (vm.sp == 0) return null;
    return vm.stack[vm.sp - 1];
}

fn lastPoped(vm: *Vm) object.Object {
    return vm.stack[vm.sp];
}

test "Integer Arithmetic" {
    const tests: []const struct {
        input: []const u8,
        expected: isize,
    } = &.{
        .{ .input = "-1", .expected = -1 },
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 2", .expected = 3 },
        .{ .input = "1 - 2", .expected = -1 },
        .{ .input = "1 * 2", .expected = 2 },
        .{ .input = "2 - 1", .expected = 1 },
        .{ .input = "2 - 1 * 2", .expected = 0 },
        .{ .input = "(2 - 1) * 2", .expected = 2 },
        .{ .input = "50 / 2 * 2 + 10 - 5", .expected = 55 },
    };

    try runVmTests(talloc, tests);
}

test "Boolean Expression" {
    const tests: []const struct {
        input: []const u8,
        expected: bool,
    } = &.{
        .{ .input = "!true", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "-1 == -1", .expected = true },
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "false == false", .expected = true },
        .{ .input = "false == true", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 > 2) != true", .expected = true },
        .{ .input = "(1 < 2) != true", .expected = false },
    };

    try runVmTests(talloc, tests);
}

fn runVmTests(alloc: anytype, tests: anytype) !void {
    for (tests) |t| {
        var lexer: Lexer = .init(t.input);
        var parser: Parser = .new(alloc, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var compiler: Compiler = .init(alloc);
        defer compiler.deinit();

        try compiler.compile(.{ .statement = .{ .program_statement = program } });
        // // assert the bytecodes
        var b = try compiler.bytecode();
        defer b.deinit(&compiler);

        var vm: Vm = .init(&b);
        try vm.run();

        const obj = vm.lastPoped();
        try expectedObject(t.expected, obj);
    }
}

fn checkIntegerObject(exp: i64, act: object.Object) !void {
    const result = switch (act) {
        .integer => |i| i,
        else => return error.NotAnInteger,
    };

    if (result.value != exp) return error.WrongIntegerValue;
}

fn checkBooleanObject(exp: bool, act: object.Object) !void {
    const result = switch (act) {
        .boolean => |i| i,
        else => return error.NotABoolean,
    };

    if (result.value != exp) return error.WrongBooleanValue;
}

fn expectedObject(expected: anytype, actual: object.Object) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int => try checkIntegerObject(@intCast(expected), actual),
        .bool => try checkBooleanObject(expected, actual),
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
