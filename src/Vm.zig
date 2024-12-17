const Vm = @This();

pub const GC_MAX = 2 * 1024;
const STACK_SIZE = 2048;
/// max(u16)
const GLOBALS_SIZE = 65536;
/// in scope objects
/// WARN: use pointers?
stack: [STACK_SIZE]Value,

gray_stack: std.ArrayList(*Object),
gray_count: usize = 0,

globals: [GLOBALS_SIZE]?Value,

bcode: *Compiler.Bytecode,

/// allocated objects list
objects: ?*Object = null,

allocator: std.mem.Allocator,

bytes_allocated: usize = 0,
// points to the next value. top is stack[sp - 1]
sp: usize = 0,

pub fn init(allocator: anytype, b: *Compiler.Bytecode) !Vm {
    var vm: Vm = .{
        .bcode = b,
        .allocator = allocator,
        .gray_stack = .init(allocator),
        .stack = undefined,
        .globals = .{null} ** GLOBALS_SIZE,
        .sp = 0,
        .bytes_allocated = 0,
    };

    for (b.constants) |value| {
        switch (value) {
            .obj => |obj| try vm.instantiateAtVm(obj),
            else => continue,
        }
    }

    return vm;
}

pub fn instantiateAtVm(vm: *Vm, obj: *Object) !void {
    vm.bytes_allocated += @sizeOf(*Object);

    if (vm.bytes_allocated > Vm.GC_MAX) {
        try memory.collectGarbage(vm);
    }

    obj.next = vm.objects;
    vm.objects = obj;
}

fn freeObjects(vm: *Vm) void {
    var obj = vm.objects;
    while (obj != null) {
        const next = obj.?.next;
        memory.freeObject(vm, obj.?);
        obj = next;
    }
}

pub fn deinit(vm: *Vm) void {
    // std.debug.print("\ndebug: allocated bytes: {}\n", .{vm.bytes_allocated});
    vm.gray_stack.deinit();
    vm.freeObjects();
}

pub fn runVm(b: *Compiler.Bytecode) !Value {
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
                var array = try vm.allocator.alloc(Value, num_elements);
                for (start_index..vm.sp) |i| {
                    array[i] = vm.stack[i];
                }
                vm.sp = vm.sp - num_elements;
                const obj = try memory.allocateObject(vm, .{ .array = array });
                try vm.push(.{ .obj = obj });
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
                try vm.push(vm.globals[global_index].?);
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
                try vm.push(.{ .boolean = if (op == .true) true else false });
            },

            .null => try vm.push(.null),

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

                if (!condition.boolean) {
                    ip = pos - 1;
                }
            },

            .pop => _ = vm.pop(),

            else => {},
        }
    }
}

fn pop(vm: *Vm) Value {
    const o = vm.stack[vm.sp - 1];
    vm.sp -= 1;
    return o;
}

fn push(vm: *Vm, obj: Value) !void {
    if (vm.sp >= STACK_SIZE) return error.VMStackOverflow;
    vm.stack[vm.sp] = obj;
    vm.sp += 1;
}

fn top(vm: *Vm) ?Value {
    if (vm.sp == 0) return null;
    return vm.stack[vm.sp - 1];
}

pub fn lastPopped(vm: *Vm) Value {
    return vm.stack[vm.sp];
}

fn executeIndexOperation(vm: *Vm, left: Value, index: Value) !void {
    if (index == .integer) {
        if (left == .obj and left.obj.type == .array) {
            const array = left.obj.type.array;
            const i = index.integer;
            const len = array.len;

            if (i >= 0 and i < len) {
                return try vm.push(array[@intCast(i)]);
            }
        }

        if (left == .obj and left.obj.type == .string) {
            const string = left.obj.type.string;
            const i = index.integer;
            const len = string.len;

            if (i >= 0 and i < len) {
                const char = string[@intCast(i)];
                const str = try vm.allocator.dupe(u8, &.{char});
                const obj = try memory.allocateObject(vm, .{ .string = str });
                return try vm.push(.{ .obj = obj });
            }
        }
        return try vm.push(.null);
    }

    return error.TypeMismatchInIndexOperation;
}

fn executePrefixOperation(vm: *Vm, op: code.Opcode) !void {
    const obj = vm.pop();

    switch (obj) {
        .integer => |integer| {
            if (op == .min) {
                return try vm.push(.{ .integer = -integer });
            }
            return error.UnknowIntegerOperation;
        },

        .float => |float| {
            if (op == .min) {
                return try vm.push(.{ .float = -float });
            }
            return error.UnknowIntegerOperation;
        },

        .boolean => |boolean| {
            if (op == .not) {
                return try vm.push(.{ .boolean = !boolean });
            }
            return error.UnknowBooleanOperation;
        },

        .null => {
            if (op == .not) {
                return try vm.push(.null);
            }
            return error.UnknowBooleanOperation;
        },

        else => return error.UnsupportedOperation,
    }
}

fn executeBinaryOperation(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    if (right == .integer and left == .integer) {
        const right_val = right.integer;
        const left_val = left.integer;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => @divTrunc(left_val, right_val),
            else => unreachable,
        };
        return try vm.push(.{ .integer = result });
    }

    if (right == .float and left == .float) {
        const right_val = right.float;
        const left_val = left.float;
        const result = switch (op) {
            .add => left_val + right_val,
            .sub => left_val - right_val,
            .mul => left_val * right_val,
            .div => left_val / right_val,
            else => unreachable,
        };
        return try vm.push(.{ .float = result });
    }

    if (right == .obj and left == .obj) {
        if (right.obj.type == .string and left.obj.type == .string) {
            const left_val = left.obj.type.string;
            const right_val = right.obj.type.string;
            if (op != .add) {
                return error.UnsupportedStringOperation;
            }
            const string = try std.mem.concat(vm.allocator, u8, &.{ left_val, right_val });
            const obj = try memory.allocateObject(vm, .{ .string = string });
            return try vm.push(.{ .obj = obj });
        }
    }

    return error.UnsupportedOperation;
}

fn executeComparison(vm: *Vm, op: code.Opcode) !void {
    const right = vm.pop();
    const left = vm.pop();

    if (right == .boolean and left == .boolean) {
        const right_val = right.boolean;
        const left_val = left.boolean;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .null or left == .null) {
        const result = switch (op) {
            .eq => right == .null and left == .null,
            .neq => right != .null or left != .null,
            else => return error.UnknowBooleanOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .integer and left == .integer) {
        const right_val = right.integer;
        const left_val = left.integer;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            else => return error.UnknowIntegerOperation,
        };
        return try vm.push(.{ .boolean = result });
    }

    if (right == .float and left == .float) {
        const right_val = right.float;
        const left_val = left.float;
        const result = switch (op) {
            .eq => left_val == right_val,
            .neq => left_val != right_val,
            .gt => left_val > right_val,
            else => return error.UnknowfloatOperation,
        };
        return try vm.push(.{ .boolean = result });
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

const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const code = @import("code.zig");
const object = @import("object.zig");
const Lexer = @import("Lexer.zig");
const Compiler = @import("Compiler.zig");
const talloc = std.testing.allocator;
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;

// test {
//     _ = @import("vm_test.zig");
// }
