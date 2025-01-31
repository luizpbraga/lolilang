const Vm = @This();

pub var GC_MAX: usize = 64 * 1024 * @sizeOf(*Object);

const FRAME_SIZE = 4 * 1024;

const STACK_SIZE = 32 * 1024;
/// max(u16)
const GLOBALS_SIZE = 65536;

constants: []Value,
/// in scope objects
stack: []Value,
/// points to the next value. top is stack[sp - 1]
sp: usize = 0,
/// Global Declared
globals: []?Value,
// frames: std.ArrayList(Frame),
frames: [FRAME_SIZE]Frame,
frames_index: usize = 1,

allocator: std.mem.Allocator,
gray_stack: std.ArrayList(*Object),
gray_count: usize = 0,
bytes_allocated: usize = 0,
positions: []usize,
cursor: usize = 0,
/// GC: allocated objects linked list
objects: ?*Object = null,
errors: *Error,
t: i68 = 0,

pub fn newError(vm: *Vm, comptime fmt: []const u8, args: anytype) anyerror {
    const pos = vm.positions[if (vm.cursor < vm.positions.len) vm.cursor else vm.positions.len - 1];
    const line: Line = .init(vm.errors.input, pos);
    try vm.errors.msg.writer().print(Error.BOLD ++ "{}:{}: ", .{ line.start, line.index });
    try vm.errors.msg.writer().print(Error.RED ++ "Runtime Error: " ++ Error.END ++ fmt ++ "\n", args);
    try vm.errors.msg.writer().print("\t{s}\n\t", .{line.line});
    try vm.errors.msg.writer().writeByteNTimes(' ', line.start);
    try vm.errors.msg.writer().writeAll("\x1b[32m^\x1b[0m\n");
    return error.Runtime;
}

pub fn init(allocator: std.mem.Allocator, b: *Compiler.Bytecode, errors: *Error) !Vm {
    var vm: Vm = .{
        .constants = b.constants,
        .positions = b.positions,
        .allocator = allocator,
        .gray_stack = try .initCapacity(allocator, FRAME_SIZE / 4),
        .stack = try allocator.alloc(Value, STACK_SIZE),
        .globals = try allocator.alloc(?Value, GLOBALS_SIZE),
        .sp = 0,
        .bytes_allocated = 0,
        .frames = undefined,
        .errors = errors,
        .t = std.time.timestamp(),
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
    vm.allocator.free(vm.stack);
    vm.allocator.free(vm.globals);
    vm.allocator.free(vm.positions);
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
pub fn run(vm: *Vm) anyerror!void {
    var ip: usize = 0;
    var instructions: code.Instructions = undefined;
    var op: code.Opcode = undefined;
    var fm = vm.currentFrame();
    while (fm.ip + 1 < fm.instructions().len) : (vm.cursor += 1) {
        fm.ip += 1;
        ip = @intCast(fm.ip);
        instructions = fm.instructions();
        op = @enumFromInt(instructions[ip]);

        switch (op) {
            .set_range => {
                const start = vm.pop();
                const end = vm.pop();

                if (start != .integer or end != .integer) {
                    return vm.newError("Invalid Range", .{});
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
                const builtin = builtins.builtin_functions[builtin_index];
                const value = try builtin.function(vm, &.{caller});
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
                vm.stack[index] = value;
            },

            .getlv => {
                const local_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const index = @as(usize, @intCast(fm.bp)) + local_index;
                const value = vm.stack[index];
                try vm.push(value);
            },

            .getbf => {
                const builtin_index = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;
                const builtin = builtins.builtin_functions[builtin_index];
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

            .eq, .neq, .gt, .gte, .land, .lor => try operation.executeComparison(vm, op),

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
                if (last_ip >= pos) {
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

                return vm.newError("Invalid Condition; expect boolean, got {s}\n", .{condition.name()});
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
                const obj = try memory.allocateObject(vm, .{ .closure = closure });
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

                const start_index = vm.sp - (fields_number - 1) * 2;
                const end_index = vm.sp;
                var i = start_index;
                struct_type.name = vm.stack[i - 1].tag;
                while (i < end_index) : (i += 2) {
                    const name = vm.stack[i].tag;
                    const value = vm.stack[i + 1];
                    try struct_type.fields.put(name, value);
                }

                const obj = try memory.allocateObject(vm, .{ .type = struct_type });
                errdefer vm.allocator.destroy(obj);
                // vm.sp -= 1;
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

                // default values
                var iter = value_type.fields.iterator();
                while (iter.next()) |entry| {
                    try instance.fields.put(entry.key_ptr.*, entry.value_ptr.*);
                }

                // user define values
                const start_index = vm.sp - fields_number * 2;
                const end_index = vm.sp;
                var i = start_index;
                // number of fields
                var f: usize = 1;
                while (i < end_index) : (i += 2) {
                    const name = vm.stack[i].tag;
                    const value = vm.stack[i + 1];
                    if (!value_type.fields.contains(name)) {
                        return vm.newError("struct {s} have no field called {s}", .{ value_type.name orelse "annon", name });
                    }
                    try instance.fields.put(name, value);
                    f += 2;
                }

                const obj = try memory.allocateObject(vm, .{ .instance = instance });
                errdefer vm.allocator.destroy(obj);
                // is not clear why, but works!
                // TODO: why reassign a field in a struct needs this?
                vm.sp -= f;

                try vm.push(.{ .obj = obj });
            },

            // TODO: better method call logic
            .call => {
                const args_number = std.mem.readInt(u8, instructions[ip + 1 ..][0..1], .big);
                fm.ip += 1;

                // the second -1: i added a null, for method call
                // const caller_value = vm.stack[vm.sp - 1 - args_number - 1];
                // if (caller_value != .obj) {
                //     caller_value = vm.stack[vm.sp - 1 - args_number];
                // }
                const caller_value = vm.stack[vm.sp - 1 - args_number];

                switch (caller_value) {
                    .obj => |ob| switch (ob.type) {
                        .desc => |func| {
                            const bp: isize = @intCast(vm.sp - args_number);

                            if (func.method) |self| {
                                if (args_number + 1 != func.num_parameters) {
                                    return vm.newError("Desc: Arguments Mismatched", .{});
                                }

                                try vm.push(.{ .obj = self });

                                // BIG FUCK ME!
                                if (args_number + 1 > 1) {
                                    const last = vm.stack[vm.sp - 1];
                                    var i = vm.sp - 1;
                                    while (i > 0) : (i -= 1) {
                                        vm.stack[i] = vm.stack[i - 1];
                                    }
                                    vm.stack[vm.sp - 1 - args_number] = last;
                                }

                                // the null, first arg will be the self obj
                                const frame: Frame = .init(.{ .func = func }, bp);
                                vm.pushFrame(frame);
                                fm = vm.currentFrame();
                                // the call stack space allocation
                                vm.sp = @as(usize, @intCast(fm.bp)) + func.num_locals;
                                continue;
                            }

                            if (args_number != func.num_parameters) {
                                return vm.newError("Function: Arguments Mismatched: expect {}, got {} ", .{ func.num_parameters, args_number });
                            }

                            const frame: Frame = .init(.{ .func = func }, bp);
                            vm.pushFrame(frame);
                            fm = vm.currentFrame();
                            // the call stack space allocation
                            vm.sp = @as(usize, @intCast(fm.bp)) + func.num_locals;
                            continue;
                        },

                        .function => |func| {
                            const bp: isize = @intCast(vm.sp - args_number);

                            if (args_number != func.num_parameters) {
                                return vm.newError("Function: Arguments Mismatched: expect {}, got {} ", .{ func.num_parameters, args_number });
                            }

                            const frame: Frame = .init(.{ .func = func }, bp);
                            vm.pushFrame(frame);
                            fm = vm.currentFrame();
                            // the call stack space allocation
                            vm.sp = @as(usize, @intCast(fm.bp)) + func.num_locals;
                            continue;
                        },

                        .closure => |cl| {
                            const func = cl.func;
                            if (args_number != func.num_parameters) {
                                return vm.newError("Closure Arguments Mismatched", .{});
                            }

                            const frame: Frame = .init(.{ .func = func }, @intCast(vm.sp - args_number));
                            vm.pushFrame(frame);
                            fm = vm.currentFrame();
                            // the call stack space allocation
                            vm.sp = @as(usize, @intCast(frame.bp)) + func.num_locals;
                            continue;
                        },

                        else => {
                            return vm.newError("Caller is not a Function: got {s}", .{caller_value.name()});
                        },
                    },

                    .builtin => |bui| {
                        const args = vm.stack[vm.sp - args_number .. vm.sp];
                        vm.sp = vm.sp - args_number - 1;
                        try vm.push(try bui.function(vm, args));
                    },

                    else => {
                        return vm.newError("Caller is not a Function: got {s}", .{caller_value.name()});
                    },
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
const Line = @import("Line.zig");
const Error = @import("Error.zig");
