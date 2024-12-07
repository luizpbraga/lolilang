const std = @import("std");
const talloc = std.testing.allocator;

/// Represents the instructions bytes
pub const Instructions = []u8;

/// Instruction frist byte.
pub const Opcode = enum(u8) {
    /// comptime numbers
    constant,

    /// numbers of operands (bytes) for a given upcode
    /// optimize: use a single small integer
    const OperandWidth = []const usize;

    /// Debug propose, []const usize represents the operand width
    const definitions: std.EnumMap(Opcode, OperandWidth) = .init(.{
        // 2 bytes long -> u16 (max of 65535 contants defined)
        // bytecode with 3 bytes long
        .constant = &.{2},
    });

    pub fn lookUp(op: u8) !OperandWidth {
        return definitions.get(@enumFromInt(op)) orelse error.UndefinedOpcode;
    }
};

/// TODO: maybe a comptime functions
/// caller owns the memory
pub fn makeBytecode(alloc: anytype, op: Opcode, operands: []const usize) ![]u8 {
    const widths = Opcode.definitions.get(op) orelse {
        return error.EmptyDefinition;
    };

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
            else => {},
        }
        offset += w;
    }

    return instructions;
}

test makeBytecode {
    const tests: []const struct {
        op: Opcode,
        operands: []const usize,
        expected: []const u8,
    } = &.{
        .{ .op = .constant, .operands = &.{65534}, .expected = &.{ @intFromEnum(Opcode.constant), 255, 254 } },
    };

    for (tests) |t| {
        const instruction = try makeBytecode(talloc, t.op, t.operands);
        defer talloc.free(instruction);

        if (instruction.len != t.expected.len) {
            std.log.err("instruction has wrong lenght. Want={}, got={}\n", .{ t.expected.len, instruction.len });
            return error.UnexpendedInstructiosLenght;
        }

        for (instruction, t.expected, 0..) |ins, bytes, i| {
            if (ins != bytes) {
                std.log.err("wrong bytes at {}. wand={}, got={}\n", .{ i, bytes, ins });
                return error.WrongByte;
            }
        }
    }
}

test readOperands {
    const tests: []const struct {
        op: Opcode,
        operands: []const usize,
        bytes_read: usize,
    } = &.{
        .{ .op = .constant, .operands = &.{65535}, .bytes_read = 2 },
    };

    for (tests) |t| {
        const instruction = try makeBytecode(talloc, t.op, t.operands);
        defer talloc.free(instruction);

        const widths = try Opcode.lookUp(@intFromEnum(t.op));

        const operands_read, const n = try readOperands(talloc, widths, instruction[1..]);
        defer talloc.free(operands_read);
        if (n != t.bytes_read) return error.WrongBytesRed;

        for (t.operands, operands_read) |want, read| {
            if (read != want) return error.OperandWrong;
        }
    }
}

/// Decode the instructions (Reverse of makeBytecode)
/// retuns the decoded operands and the readed bytes to decode;
fn readOperands(alloc: anytype, widths: Opcode.OperandWidth, ins: Instructions) !struct { []usize, usize } {
    const operands = try alloc.alloc(usize, widths.len);
    var offset: usize = 0;

    for (widths, 0..) |w, i| {
        switch (w) {
            2 => {
                // u16: 2 bytes
                operands[i] = @intCast(std.mem.readInt(u16, ins[offset..][0..2], .big));
            },
            else => {},
        }
        offset += @intCast(w);
    }

    return .{ operands, offset };
}

test "Instructions String" {
    const instructions: []const Instructions = &.{
        try makeBytecode(talloc, .constant, &.{1}),
        try makeBytecode(talloc, .constant, &.{2}),
        try makeBytecode(talloc, .constant, &.{65535}),
    };
    defer for (instructions) |ins| talloc.free(ins);

    const expected =
        \\0000 constant 1
        \\0003 constant 2
        \\0006 constant 65535
        \\
    ;

    const concatted = try std.mem.concat(talloc, u8, instructions);
    defer talloc.free(concatted);

    const formatted_string = try formatInstruction(talloc, concatted);
    defer talloc.free(formatted_string);

    if (!std.mem.eql(u8, expected, formatted_string)) {
        std.log.err("expect:'{s}'\ngot:'{s}'\n", .{ expected, formatted_string });
        return error.InstructionsWronglyFormatted;
    }
}

/// Dissemble the bytecodes
fn formatInstruction(alloc: anytype, ins: Instructions) ![]const u8 {
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
            1 => try out.writer().print("{:0>4} {s} {d}\n", .{ i, @tagName(op), operands[0] }),
            else => try out.writer().print("{d} Error: operand len {d} does not match defined {d}\n", .{ i, operands.len, operand_count }),
        }

        i += 1 + read;
    }

    return try out.toOwnedSlice();
}
