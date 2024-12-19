const Vm = @This();

pub const GC_MAX = 2 * 1024;

const FRAME_SIZE = 1024;

const STACK_SIZE = 2048;
/// max(u16)
const GLOBALS_SIZE = 65536;

constants: []Value,
/// in scope objects
stack: [STACK_SIZE]Value,
/// points to the next value. top is stack[sp - 1]
sp: usize = 0,
/// Global Declared
globals: [GLOBALS_SIZE]?Value,
// frames: std.ArrayList(Frame),
frames: [FRAME_SIZE]Frame,
frames_index: usize = 1,

allocator: std.mem.Allocator,
gray_stack: std.ArrayList(*Object),
gray_count: usize = 0,
bytes_allocated: usize = 0,
/// GC: allocated objects linked list
objects: ?*Object = null,

pub fn init(allocator: anytype, b: *Compiler.Bytecode) !Vm {
    var vm: Vm = .{
        .constants = b.constants,
        .allocator = allocator,
        .gray_stack = try .initCapacity(allocator, 100),
        .stack = .{.null} ** STACK_SIZE,
        .globals = .{null} ** GLOBALS_SIZE,
        .sp = 0,
        .bytes_allocated = 0,
        .frames = undefined,
    };

    // main frame
    vm.frames[0] = .init(.{ .instructions = b.instructions }, 0);

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
    vm.gray_stack.deinit();
    vm.freeObjects();
}

fn currentFrame(vm: *Vm) *Frame {
    return &vm.frames[vm.frames_index - 1];
}

fn pushFrame(vm: *Vm, f: Frame) void {
    vm.frames[vm.frames_index] = f;
    vm.frames_index += 1;
}

fn popFrame(vm: *Vm) *Frame {
    vm.frames_index -= 1;
    return &vm.frames[vm.frames_index];
}

/// decode cycle
pub fn run(vm: *Vm) !void {
    var ip: usize = 0;
    var instructions: code.Instructions = undefined;
    var op: code.Opcode = undefined;

    while (vm.currentFrame().ip + 1 < vm.currentFrame().instructions().len) {
        vm.currentFrame().ip += 1;
        ip = @intCast(vm.currentFrame().ip);
        instructions = vm.currentFrame().instructions();
        op = @enumFromInt(instructions[ip]);

        switch (op) {
            .array => {
                const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                const start_index = vm.sp - num_elements;
                const end_index = vm.sp;

                var array = try vm.allocator.alloc(Value, end_index - start_index);
                for (start_index..end_index) |i| {
                    array[i - start_index] = vm.stack[i];
                }
                vm.sp = vm.sp - num_elements;
                const obj = try memory.allocateObject(vm, .{ .array = array });
                errdefer vm.allocator.destroy(obj);

                try vm.push(.{ .obj = obj });
            },

            .index => {
                const index = vm.pop();
                const left = vm.pop();
                try operation.executeIndex(vm, left, index);
            },

            .setgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                vm.globals[global_index] = vm.pop();
            },

            .setlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const frame = vm.currentFrame();
                vm.stack[@as(usize, @intCast(frame.bp)) + local_index] = vm.pop();
            },

            .getgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                try vm.push(vm.globals[global_index].?);
            },

            .getlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const frame = vm.currentFrame();
                try vm.push(vm.stack[@as(usize, @intCast(frame.bp)) + local_index]);
            },

            .constant => {
                const const_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                try vm.push(vm.constants[const_index]);
            },

            .retv => {
                const returned_value = vm.pop(); // get the return value
                const frame = vm.popFrame(); // return to the calle frame
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                // _ = vm.pop(); // pop the fn
                try vm.push(returned_value); // push the return value
            },

            .retn => {
                const frame = vm.popFrame(); // return to the calle frame
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                try vm.push(.null); // push the return value
            },

            .add, .sub, .mul, .div => try operation.executeBinary(vm, op),

            .eq, .neq, .gt => try operation.executeComparison(vm, op),

            .not, .min => try operation.executePrefix(vm, op),

            .true, .false => {
                try vm.push(.{ .boolean = if (op == .true) true else false });
            },

            .null => try vm.push(.null),

            .jump => {
                // get the operand located right afther the opcode
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                // move ip to the target out of jump
                vm.currentFrame().ip = pos - 1;
            },

            .jumpifnottrue => {
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;

                const condition = vm.pop();

                if (condition != .boolean) {
                    return error.NotABooleanExpression;
                }

                if (!condition.boolean) {
                    vm.currentFrame().ip = pos - 1;
                }
            },

            .pop => _ = vm.pop(),

            .getbf => {
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const builtin = builtins.list[builtin_index];
                try vm.push(.{ .builtin = builtin });
            },

            .method => {
                const caller = vm.pop();
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const builtin = builtins.list[builtin_index];
                const value = builtin.function(&.{caller});
                try vm.push(value);
            },

            .call => {
                const args_number = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const value = vm.stack[vm.sp - 1 - args_number];

                switch (value) {
                    .obj => |ob| {
                        if (ob.type != .function) {
                            return error.CallerIsNoFunction;
                        }

                        const func = ob.type.function;

                        if (args_number != func.num_parameters) {
                            return error.ArgumentsMismatch;
                        }

                        const frame: Frame = .init(func, @intCast(vm.sp - args_number));
                        vm.pushFrame(frame);
                        // the call stack space allocation
                        vm.sp = @as(usize, @intCast(frame.bp)) + func.num_locals;
                    },

                    .builtin => |bui| {
                        const args = vm.stack[vm.sp - args_number .. vm.sp];
                        vm.sp = vm.sp - args_number - 1;
                        try vm.push(bui.function(args));
                    },

                    else => return error.ValueNotCallable,
                }
            },

            // else => {
            //     std.debug.print("opcode: {}", .{op});
            //     return error.UnkowOpcode;
            // },
        }
    }
}

pub fn pop(vm: *Vm) Value {
    const o = vm.stack[vm.sp - 1];
    vm.sp -= 1;
    return o;
}

pub fn push(vm: *Vm, obj: Value) !void {
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

const std = @import("std");
const code = @import("code.zig");
const Compiler = @import("Compiler.zig");
const memory = @import("memory.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const operation = @import("operation.zig");
const Frame = @import("Frame.zig");
const builtins = @import("builtins.zig");

// test {
//     _ = @import("vm_test.zig");
// }
