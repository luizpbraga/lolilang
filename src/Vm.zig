const Vm = @This();

const STACK_SIZE = 2048;

stack: [STACK_SIZE]object.Object,
bcode: *Compiler.Bytecode,
// points to the next value. top is stack[sp - 1]
sp: usize = 0,

pub fn init(b: *Compiler.Bytecode) Vm {
    return .{ .bcode = b, .stack = undefined };
}

pub fn runVm(b: *Compiler.Bytecode) !object.Object {
    var vm: Vm = .init(b);
    try vm.run();
    return vm.lastPoped();
}

pub fn run(vm: *Vm) !void {
    // decode cycle
    const instructions = vm.bcode.instructions;
    var ip: usize = 0;
    while (ip < instructions.len) : (ip += 1) {
        const op: code.Opcode = @enumFromInt(instructions[ip]);
        switch (op) {
            .constant => {
                const const_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
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

pub fn lastPoped(vm: *Vm) object.Object {
    return vm.stack[vm.sp];
}

const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const talloc = std.testing.allocator;

test {
    _ = @import("vm_test.zig");
}
