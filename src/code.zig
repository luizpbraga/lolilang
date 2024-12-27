const std = @import("std");
const talloc = std.testing.allocator;

/// Represents the instructions bytes
pub const Instructions = []u8;

/// Instruction frist byte.
pub const Opcode = enum(u8) {
    /// comptime values
    constant,
    /// set a global constant on the stack
    setgv,
    /// get a global constant on the stack
    getgv,
    /// set a local constant on the stack
    setlv,
    /// get a local constant on the stack
    getlv,
    /// remove the topmost elements of the stack and add it
    add,
    /// remove the topmost elements of the stack and sub it
    sub,
    /// remove the topmost elements of the stack and mul it
    mul,
    /// remove the topmost elements of the stack and div it
    div,
    /// pops the topmost stack element after every statement
    pop,
    /// boolean values: add boolean value 'true' to the stack
    true,
    /// boolean values: add boolean value 'true' to the stack
    false,
    /// puts a null object on the stack
    null,
    /// compare the two topmost op on the stack (==)
    eq,
    /// compare the two topmost op on the stack (> and <)
    gt,
    /// compare the two topmost op on the stack (!=)
    neq,
    /// infix op (-)
    min,
    /// infix op (!)
    not,
    /// conditionals: jump if the value on top of the stack IS NOT true
    jumpifnottrue,
    /// just jump bytecodes
    jump,
    /// add an array
    array,
    /// add an hashmap
    hash,
    /// index operation
    index_set,
    index_get,
    /// get the function and execute it on the top of the stack
    call,
    /// return a value on top of the stack
    retv,
    /// return nothing (null), just go back
    retn,
    /// builtin methods
    getbf,
    /// TODO: add description
    method,
    /// TODO: add description
    brk,

    set_range,
    get_range,

    /// numbers of operands (bytes) for a given upcode
    /// optimize: use a single small integer
    pub const OperandWidth = []const usize;

    /// Debug propose, []const usize represents the operand width
    pub const definitions: std.EnumMap(Opcode, OperandWidth) = .initFullWithDefault(&.{}, .{
        // 2 bytes long -> u16 (max of 65535 contants defined)
        // bytecode with 3 bytes long
        .constant = &.{2},
        .setgv = &.{2},
        .getgv = &.{2},
        .setlv = &.{1},
        .get_range = &.{1},
        .set_range = &.{1},
        .getlv = &.{1},
        .call = &.{1},
        .method = &.{1},
        .getbf = &.{1},
        .jumpifnottrue = &.{2},
        .jump = &.{2},
        // the array lenght is the with
        .array = &.{2},
        .hash = &.{2},
    });

    pub fn lookUp(op: u8) !OperandWidth {
        return definitions.get(@enumFromInt(op)) orelse error.UndefinedOpcode;
    }
};

/// TODO: maybe a comptime functions
/// caller owns the memory
pub fn makeBytecode(alloc: anytype, op: Opcode, operands: []const usize) ![]u8 {
    const widths = Opcode.definitions.get(op).?;

    var instruction_len: usize = 1;
    for (widths) |w| {
        instruction_len += @intCast(w);
    }

    const instructions = try alloc.alloc(u8, instruction_len);
    errdefer alloc.free(instructions);
    // fist instruction: the opcode
    instructions[0] = @intFromEnum(op);

    var offset: usize = 1;
    for (operands, 0..) |o, i| {
        const w = widths[i];
        switch (w) {
            2 => {
                // 2 bytes -> u16
                const native = std.mem.nativeTo(u16, @as(u16, @intCast(o)), .big);
                const bytes = std.mem.asBytes(&native);
                for (0.., bytes) |idx, by| {
                    instructions[offset + idx] = by;
                }
            },

            1 => {
                const native = std.mem.nativeTo(u8, @as(u8, @intCast(o)), .big);
                const bytes = std.mem.asBytes(&native);
                for (0.., bytes) |idx, by| {
                    instructions[offset + idx] = by;
                }
            },

            else => {},
        }
        offset += w;
    }

    return instructions;
}

/// Decode the instructions (Reverse of makeBytecode)
/// retuns the decoded operands and the readed bytes to decode;
pub fn readOperands(alloc: anytype, widths: Opcode.OperandWidth, ins: Instructions) !struct { []usize, usize } {
    const operands = try alloc.alloc(usize, widths.len);
    var offset: usize = 0;

    for (widths, 0..) |w, i| {
        switch (w) {
            2 => {
                // u16: 2 bytes
                operands[i] = @intCast(std.mem.readInt(u16, ins[offset..][0..2], .big));
            },

            1 => {
                // u8: 1 bytes
                operands[i] = @intCast(std.mem.readInt(u8, ins[offset..][0..1], .big));
            },
            else => {},
        }
        offset += @intCast(w);
    }

    return .{ operands, offset };
}

/// Dissemble the bytecodes
pub fn formatInstruction(alloc: anytype, ins: Instructions) ![]const u8 {
    var out: std.ArrayList(u8) = .init(alloc);
    errdefer out.deinit();

    var i: usize = 0;
    while (i < ins.len) {
        const op_int = ins[i];
        const w = Opcode.lookUp(op_int) catch |err| {
            try out.writer().print("error: {s}\n", .{@errorName(err)});
            continue;
        };
        const op: Opcode = @enumFromInt(op_int);

        const operands, const read = try readOperands(alloc, w, ins[i + 1 ..]);
        defer alloc.free(operands);

        const operand_count = w.len;
        if (operands.len != operand_count) {
            try out.writer().print("{} Error: openand lenght {} does not match definied {}\n", .{ i, operands.len, operand_count });
        }

        switch (operand_count) {
            0 => try out.writer().print("{:0>4} {s}\n", .{ i, @tagName(op) }),
            1 => try out.writer().print("{:0>4} {s} {d}\n", .{ i, @tagName(op), operands[0] }),
            2 => try out.writer().print("{:0>4} {s} {d} {d}\n", .{ i, @tagName(op), operands[0], operands[1] }),
            3 => try out.writer().print("{:0>4} {s} {d} {d} {d}\n", .{ i, @tagName(op), operands[0], operands[1], operands[2] }),
            else => try out.writer().print("{d} Error: operand len {d} does not match defined {d}\n", .{ i, operands.len, operand_count }),
        }

        i += 1 + read;
    }

    return try out.toOwnedSlice();
}

test {
    _ = @import("./code_test.zig");
}
