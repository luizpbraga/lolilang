const Vm = @This();

pub const GC_MAX = 2 * 1024;

const FRAME_SIZE = 1024;

const STACK_SIZE = 2 * 1024;
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
            .set_range => {
                const pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const value = try vm.pop();
                vm.constants[pos] = .{ .range = value.toRange() };
            },

            .get_range => {
                const pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                var range = &vm.constants[pos].range;

                const value = range.next() orelse {
                    try vm.push(.null);
                    try vm.push(.{ .boolean = false });
                    continue;
                };
                // _ = try vm.pop();
                try vm.push(value);
                try vm.push(.{ .boolean = true });
            },

            // .array => {
            //     const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
            //     vm.currentFrame().ip += 2;
            //     const start_index = vm.sp - num_elements;
            //     const end_index = vm.sp;
            //
            //     var array = try vm.allocator.alloc(Value, end_index - start_index);
            //     for (start_index..end_index) |i| {
            //         array[i - start_index] = vm.stack[i];
            //     }
            //
            //     vm.sp = vm.sp - num_elements;
            //
            //     const obj = try memory.allocateObject(vm, .{ .array = array });
            //     errdefer vm.allocator.destroy(obj);
            //
            //     try vm.push(.{ .obj = obj });
            // },

            .array => {
                const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                const start_index = vm.sp - num_elements;
                const end_index = vm.sp;

                var array = try std.ArrayList(Value).initCapacity(vm.allocator, end_index - start_index);
                for (start_index..end_index) |i| {
                    try array.append(vm.stack[i]);
                }

                vm.sp = vm.sp - num_elements;

                const obj = try memory.allocateObject(vm, .{ .array = array });
                errdefer vm.allocator.destroy(obj);

                try vm.push(.{ .obj = obj });
            },

            .hash => {
                const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;

                var hash: Object.Hash = .{
                    .pairs = .init(vm.allocator),
                };

                const start_index = vm.sp - num_elements;
                const end_index = vm.sp;
                var i = start_index;
                while (i < end_index) : (i += 2) {
                    const key_value = vm.stack[i];
                    const value_value = vm.stack[i + 1];

                    const hash_key: Object.Hash.Key = try .init(&key_value);

                    const hash_pair: Object.Hash.Pair = .{
                        .key = key_value,
                        .value = value_value,
                    };

                    try hash.pairs.put(hash_key, hash_pair);
                }

                vm.sp = vm.sp - num_elements;
                const obj = try memory.allocateObject(vm, .{ .hash = hash });
                errdefer vm.allocator.destroy(obj);

                try vm.push(.{ .obj = obj });
            },

            .index_get => {
                const index = try vm.pop();
                const left = try vm.pop();
                try operation.executeIndex(vm, &left, &index);
            },

            .method_set => {
                const field_pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const value = try vm.pop();
                var left = try vm.pop();
                const index = vm.constants[field_pos];
                try operation.setIndex(vm, &left, index, value);
            },

            .index_set => {
                const value = try vm.pop();
                const index = try vm.pop();
                var left = try vm.pop();
                try operation.setIndex(vm, &left, index, value);
            },

            .setgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                const value = try vm.pop();
                vm.globals[global_index] = value;
            },

            .setlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const frame = vm.currentFrame();
                const value = try vm.pop();
                vm.stack[@as(usize, @intCast(frame.bp)) + local_index] = value;
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
                const value = vm.stack[@as(usize, @intCast(frame.bp)) + local_index];
                // std.debug.print("local {}\n", .{value});
                try vm.push(value);
            },

            .constant => {
                const const_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                try vm.push(vm.constants[const_index]);
            },

            .retv => {
                const returned_value = try vm.pop(); // get the return value
                const frame = vm.popFrame(); // return to the callee frame
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                // _ = try vm.pop(); // pop the fn
                try vm.push(returned_value); // push the return value
            },

            .retn => {
                const frame = vm.popFrame(); // return to the callee frame
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                try vm.push(.null); // push the return value
            },

            .brk => {
                // ??????????????????????????????????????????????????/
            },

            .add, .sub, .mul, .div, .mod => try operation.executeBinary(vm, op),

            .eq, .neq, .gt, .gte => try operation.executeComparison(vm, op),

            .not, .min => try operation.executePrefix(vm, op),

            .true => try vm.push(.{ .boolean = true }),

            .false => try vm.push(.{ .boolean = false }),

            .null => try vm.push(.null),

            .jump => {
                const last_ip = vm.currentFrame().ip;
                // get the operand located right after the opcode
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                // move ip to the target out of jump
                vm.currentFrame().ip = @as(isize, @intCast(pos)) - 1;

                // the 'if' don't need to pop, just the 'for' loop, so we compare
                // the last ip value with the current jump position;.
                // If ip is bigger (for loop) we pop the null value. This prevent accumulating nulls in the stack
                if (last_ip > pos) {
                    _ = try vm.pop();
                }
            },

            .jumpifnottrue => {
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                vm.currentFrame().ip += 2;
                const condition = try vm.pop();

                if (condition == .boolean) {
                    if (!condition.boolean) vm.currentFrame().ip = pos - 1;
                    continue;
                }

                std.debug.print("\ngot {}\n", .{condition});
                return error.InvalidCondition;
            },

            .pop => _ = try vm.pop(),

            .getbf => {
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                vm.currentFrame().ip += 1;
                const builtin = builtins.list[builtin_index];
                try vm.push(.{ .builtin = builtin });
            },

            .method => {
                const caller = try vm.pop();
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

                        // const fmt = try code.formatInstruction(vm.allocator, func.instructions);
                        // defer vm.allocator.free(fmt);
                        // std.debug.print("\n{s}", .{fmt});

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
        // std.debug.print("{any}\n", .{vm.stack[0..vm.sp]});
    }
}

pub fn pop(vm: *Vm) !Value {
    //if (vm.sp == 0) return .null;
    const o = vm.stack[try std.math.sub(usize, vm.sp, 1)];
    vm.sp -= 1;
    return o;
}

pub fn push(vm: *Vm, obj: Value) !void {
    if (vm.sp >= STACK_SIZE) return error.VMStackOverflow;
    vm.stack[vm.sp] = obj;
    vm.sp += 1;
}

fn top(vm: *Vm) !?Value {
    if (vm.sp == 0) return null;
    return vm.stack[try std.math.sub(usize, vm.sp, 1)];
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
