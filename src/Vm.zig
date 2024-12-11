const Vm = @This();

const STACK_SIZE = 2048;
/// max(u16)
const GLOBALS_SIZE = 65536;
/// in scope objects
/// WARN: use pointers?
stack: [STACK_SIZE]object.Object,
globals: [GLOBALS_SIZE]object.Object,
bcode: *Compiler.Bytecode,
// points to the next value. top is stack[sp - 1]
sp: usize,
/// TODO: When the GC is implemented, this must be deleted
arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator),

pub fn init(b: *Compiler.Bytecode) Vm {
    return .{ .bcode = b, .stack = undefined, .globals = undefined, .sp = 0 };
}

pub fn deinit(vm: *Vm) void {
    vm.arena.deinit();
}

pub fn runVm(b: *Compiler.Bytecode) !object.Object {
    var vm: Vm = .init(b);
    try vm.run();
    return vm.lastPopped();
}

pub fn run(vm: *Vm) !void {
    // decode cycle
    const instructions = vm.bcode.instructions;
    var ip: usize = 0;
    while (ip < instructions.len) : (ip += 1) {
        const op: code.Opcode = @enumFromInt(instructions[ip]);
        switch (op) {
            .array => {
                const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                ip += 2;
                const start_index = vm.sp - num_elements;
                const allocator = vm.arena.allocator();
                var elements: std.ArrayList(Object) = .init(allocator);
                errdefer elements.deinit();
                for (start_index..vm.sp) |i| {
                    try elements.append(vm.stack[i]);
                }
                vm.sp = vm.sp - num_elements;
                try vm.push(.{ .array = .{ .elements = try elements.toOwnedSlice() } });
            },

            .index => {
                const index = vm.pop();
                const left = vm.pop();
                try vm.executeIndexOperation(left, index);
            },

            .setgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                ip += 2;
                vm.globals[global_index] = vm.pop();
            },

            .getgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                ip += 2;
                try vm.push(vm.globals[global_index]);
            },

            .constant => {
                const const_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                ip += 2;
                try vm.push(vm.bcode.constants[const_index]);
            },

            .add, .sub, .mul, .div => try vm.executeBinaryOperation(op),

            .eq, .neq, .gt => try vm.executeComparison(op),

            .not, .min => try vm.executePrefixOperation(op),

            .true, .false => {
                const obj: Object = if (op == .true) object.TRUE else object.FALSE;
                try vm.push(obj);
            },

            .null => try vm.push(object.NULL),

            .jump => {
                // get the operand located right afther the opcode
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                // move ip to the target out of jump
                ip = pos - 1;
            },

            .jumpifnottrue => {
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                ip += 2;

                const condition = vm.pop();

                if (condition != .boolean) {
                    return error.NotABooleanExpression;
                }

                if (!condition.boolean.value) {
                    ip = pos - 1;
                }
            },

            .pop => _ = vm.pop(),
        }
    }
}

fn executeIndexOperation(vm: *Vm, left: Object, index: Object) !void {
    if (index.objType() == .integer) {
        if (left.objType() == .array) {
            const array = left.array;
            const i = index.integer.value;
            const len = array.elements.len;

            if (i >= 0 and i < len) {
                return try vm.push(left.array.elements[@intCast(i)]);
            }
        }

        if (left.objType() == .string) {
            const string = left.string;
            const i = index.integer.value;
            const len = string.value.len;

            if (i >= 0 and i < len) {
                const char = left.string.value[@intCast(i)..@intCast(i)];
                return try vm.push(.{ .string = .{ .value = char } });
            }
        }
        return try vm.push(object.NULL);
    }

    return error.TypeMismatchInIndexOperation;
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

        .float => {
            if (op == .min) {
                return try vm.push(.{ .float = .{ .value = -obj.float.value } });
            }
            return error.UnknowIntegerOperation;
        },

        .boolean => {
            if (op == .not) {
                return try vm.push(.{ .boolean = .{ .value = !obj.boolean.value } });
            }
            return error.UnknowBooleanOperation;
        },

        .null => {
            if (op == .not) {
                return try vm.push(.{ .boolean = .{ .value = true } });
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
            else => unreachable,
        };
        return try vm.push(.{ .integer = .{ .value = result } });
    }

    if (rtype == .float and ltype == .float) {
        const right_val = right.float.value;
        const left_val = left.float.value;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => left_val / right_val,
            else => unreachable,
        };
        return try vm.push(.{ .float = .{ .value = result } });
    }

    if (rtype == .string and ltype == .string) {
        const right_val = right.string.value;
        const left_val = left.string.value;
        if (op != .add) {
            return error.UnsupportedStringOperation;
        }
        const allocator = vm.arena.allocator();
        const bytes = try std.mem.concat(allocator, u8, &.{ left_val, right_val });
        return try vm.push(.{ .string = .{ .value = bytes } });
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

    if (rtype == .null or ltype == .null) {
        const result = switch (op) {
            .eq => rtype == .null and ltype == .null,
            .neq => rtype != .null or ltype != .null,
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

    if (rtype == .float and ltype == .float) {
        const right_val = right.float.value;
        const left_val = left.float.value;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            else => return error.UnknowfloatOperation,
        };
        return try vm.push(.{ .boolean = .{ .value = result } });
    }

    // if (rtype == .float and ltype == .integer) {
    //     const right_val = right.float.value;
    //     const left_val = left.integer.value;
    //     const result = switch (op) {
    //         .eq => left_val == right_val,
    //         .neq => left_val != right_val,
    //         .gt => left_val > right_val,
    //         else => return error.UnknowfloatOperation,
    //     };
    //     return try vm.push(.{ .boolean = .{ .value = result } });
    // }
    //
    // if (rtype == .integer and ltype == .float) {
    //     const right_val = right.integer.value;
    //     const left_val = left.float.value;
    //     const result = switch (op) {
    //         .eq => left_val == right_val,
    //         .neq => left_val != right_val,
    //         .gt => left_val > right_val,
    //         else => return error.UnknowfloatOperation,
    //     };
    //     return try vm.push(.{ .boolean = .{ .value = result } });
    // }

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

pub fn lastPopped(vm: *Vm) object.Object {
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
const Object = object.Object;

test {
    _ = @import("vm_test.zig");
}
