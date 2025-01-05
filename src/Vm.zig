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
    vm.frames[0] = .init(.{ .func = .{ .instructions = b.instructions } }, 0);

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

// return a pointer?
pub fn pop(vm: *Vm) Value {
    //if (vm.sp == 0) return .null;
    const o = vm.stack[vm.sp - 1];
    vm.sp -= 1;
    return o;
}

// return a pointer?
pub fn pop2(vm: *Vm) void {
    vm.sp -= 1;
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

    var fm = vm.currentFrame();
    while (fm.ip + 1 < fm.instructions().len) {
        fm.ip += 1;
        ip = @intCast(fm.ip);
        instructions = fm.instructions();
        op = @enumFromInt(instructions[ip]);

        switch (op) {
            .set_range => {
                const start = vm.pop();
                const end = vm.pop();

                if (start != .integer or end != .integer) {
                    return error.InvalidRange;
                }

                try vm.push(.{ .range = .{
                    .start = @intCast(start.integer),
                    .end = @intCast(end.integer),
                    .value = &.null,
                } });
            },

            .to_range => {
                const pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const value = vm.pop();
                vm.constants[pos] = .{ .range = value.toRange() };
            },

            .get_range => {
                const pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                var range = &vm.constants[pos].range;

                const value = range.next() orelse {
                    try vm.push(.null);
                    try vm.push(.{ .boolean = false });
                    continue;
                };
                try vm.push(value);
                try vm.push(.{ .boolean = true });
            },

            .array => {
                const num_elements = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                fm.ip += 2;
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
                fm.ip += 2;

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

            .method_set => {
                const field_pos = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const value = vm.pop();
                var left = vm.pop();
                const index = vm.constants[field_pos];
                try operation.setIndex(vm, &left, index, value);
            },

            .method_get => {
                const caller = vm.pop();
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const builtin = builtins.list[builtin_index];
                const value = builtin.function(vm, &.{caller});
                try vm.push(value);
            },

            .index_set => {
                const value = vm.pop();
                const index = vm.pop();
                var left = vm.pop();
                try operation.setIndex(vm, &left, index, value);
            },

            .index_get => {
                const index = vm.pop();
                const left = vm.pop();
                try operation.executeIndex(vm, &left, &index);
            },

            .setgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                fm.ip += 2;
                const value = vm.pop();
                vm.globals[global_index] = value;
            },

            .getgv => {
                const global_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                fm.ip += 2;
                try vm.push(vm.globals[global_index].?);
            },

            .setlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const value = vm.pop();
                const index = @as(usize, @intCast(fm.bp)) + local_index;
                // std.debug.print("{} on {}\n", .{ value, index });
                vm.stack[index] = value;
            },

            .getlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const index = @as(usize, @intCast(fm.bp)) + local_index;
                const value = vm.stack[index];
                // std.debug.print("{} on {}\n", .{ value, index });
                try vm.push(value);
            },

            .getbf => {
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const builtin = builtins.list[builtin_index];
                try vm.push(.{ .builtin = builtin });
            },

            .constant => {
                const const_index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                fm.ip += 2;
                try vm.push(vm.constants[const_index]);
            },

            .retv => {
                const returned_value = vm.pop(); // get the return value
                const frame = vm.popFrame(); // return to the callee frame
                fm = vm.currentFrame(); // ?
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                // _ = vm.pop(); // pop the fn
                try vm.push(returned_value); // push the return value
            },

            .retn => {
                const frame = vm.popFrame(); // return to the callee frame
                fm = vm.currentFrame();
                vm.sp = @intCast(frame.bp - 1); // pop the fn
                try vm.push(.null); // push the return value
            },

            .add, .sub, .mul, .div, .mod => try operation.executeBinary(vm, op),

            .eq, .neq, .gt, .gte => try operation.executeComparison(vm, op),

            .not, .min => try operation.executePrefix(vm, op),

            .true => try vm.push(.{ .boolean = true }),

            .false => try vm.push(.{ .boolean = false }),

            .null => try vm.push(.null),

            .jump => {
                const last_ip = fm.ip;
                // get the operand located right after the opcode
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                // move ip to the target out of jump
                fm.ip = @as(isize, @intCast(pos)) - 1;

                // the 'if' don't need to pop, just the 'for' loop, so we compare
                // the last ip value with the current jump position;.
                // If ip is bigger (for loop) we pop the null value. This prevent accumulating nulls in the stack
                if (last_ip > pos) {
                    vm.pop2();
                }
            },

            .jumpifnottrue => {
                const pos = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                fm.ip += 2;
                const condition = vm.pop();

                if (condition == .boolean) {
                    if (!condition.boolean) fm.ip = pos - 1;
                    continue;
                }

                std.debug.print("\ngot {}\n", .{condition});
                return error.InvalidCondition;
            },

            .pop => vm.pop2(),

            .getfree => {
                const free_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const closure = fm.cl;
                if (closure.free.len != 0) try vm.push(closure.free[free_index]);
            },

            // this is used for recursive closure
            .current_closure => {
                const closure = fm.cl;
                var obj = try vm.allocator.create(Object);
                obj.type.closure = closure;
                // SHIT
                try vm.instantiateAtVm(obj);
                try vm.push(.{ .obj = obj });
            },

            .closure => {
                const index = std.mem.readInt(u16, instructions[ip + 1 ..][0..2], .big);
                const num_free = std.mem.readInt(u8, instructions[ip + 3 ..][0..1], .big);
                fm.ip += 3;

                var closure = &vm.constants[index];
                var frees = try vm.allocator.alloc(Value, num_free);
                errdefer vm.allocator.free(frees);

                for (0..num_free) |i| {
                    frees[i] = vm.stack[vm.sp - num_free + i];
                }
                vm.sp = vm.sp - num_free;

                closure.obj.type.closure.free = frees;

                try vm.push(closure.*);
            },

            .type => {
                const index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                const fields_number = std.mem.readInt(u8, instructions[ip + 2 ..][0..1], .big);
                const type_type = std.mem.readInt(u8, instructions[ip + 3 ..][0..1], .big);

                fm.ip += 3;

                var struct_type: Object.BuiltinType = .{
                    .index = index,
                    .type = @enumFromInt(type_type),
                    .fields = .init(vm.allocator),
                };
                errdefer struct_type.fields.deinit();

                const start_index = vm.sp - fields_number * 2;
                const end_index = vm.sp;
                var i = start_index;
                while (i < end_index) : (i += 2) {
                    const name = vm.stack[i].tag;
                    const value = vm.stack[i + 1];
                    try struct_type.fields.put(name, value);
                }

                const obj = try memory.allocateObject(vm, .{ .type = struct_type });
                errdefer vm.allocator.destroy(obj);
                try vm.push(.{ .obj = obj });
            },

            .instance => {
                const fields_number = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;

                const value_type = &vm.stack[vm.sp - 1 - fields_number * 2].obj.type.type;

                var instance: Object.Instance = .{
                    .type = value_type,
                    .fields = .init(vm.allocator),
                };
                errdefer instance.fields.deinit();

                var iter = value_type.fields.iterator();
                while (iter.next()) |entry| {
                    try instance.fields.put(entry.key_ptr.*, entry.value_ptr.*);
                }

                const start_index = vm.sp - fields_number * 2;
                const end_index = vm.sp;
                var i = start_index;
                while (i < end_index) : (i += 2) {
                    const name = vm.stack[i].tag;
                    const value = vm.stack[i + 1];
                    try instance.fields.put(name, value);
                }

                const obj = try memory.allocateObject(vm, .{ .instance = instance });
                errdefer vm.allocator.destroy(obj);
                try vm.push(.{ .obj = obj });
            },

            .call => {
                const args_number = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const value = vm.stack[vm.sp - 1 - args_number];

                switch (value) {
                    .obj => |ob| {
                        if (ob.type == .function) {
                            const func = ob.type.function;

                            if (args_number != func.num_parameters) {
                                return error.ArgumentsMismatch;
                            }

                            const frame: Frame = .init(.{ .func = func }, @intCast(vm.sp - args_number));
                            vm.pushFrame(frame);
                            fm = vm.currentFrame();
                            // the call stack space allocation
                            vm.sp = @as(usize, @intCast(frame.bp)) + func.num_locals;
                            continue;
                        }

                        if (ob.type == .closure) {
                            const func = ob.type.closure.func;

                            if (args_number != func.num_parameters) {
                                return error.ArgumentsMismatch;
                            }

                            const frame: Frame = .init(.{ .func = func }, @intCast(vm.sp - args_number));
                            vm.pushFrame(frame);
                            fm = vm.currentFrame();
                            // the call stack space allocation
                            vm.sp = @as(usize, @intCast(frame.bp)) + func.num_locals;
                            continue;
                        }

                        std.debug.print("got {}", .{ob});

                        return error.CallerIsNoFunction;
                    },

                    .builtin => |bui| {
                        const args = vm.stack[vm.sp - args_number .. vm.sp];
                        vm.sp = vm.sp - args_number - 1;
                        try vm.push(bui.function(vm, args));
                    },

                    else => return error.ValueNotCallable,
                }
            },
        }
    }
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
