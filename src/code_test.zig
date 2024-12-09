const std = @import("std");
const code = @import("code.zig");
const Opcode = code.Opcode;
const Instructions = code.Instructions;
const makeBytecode = code.makeBytecode;
const readOperands = code.readOperands;
const formatInstruction = code.formatInstruction;
const talloc = std.testing.allocator;

test makeBytecode {
    const tests: []const struct {
        op: Opcode,
        operands: []const usize,
        expected: []const u8,
    } = &.{
        .{ .op = .constant, .operands = &.{65534}, .expected = &.{ @intFromEnum(Opcode.constant), 255, 254 } },
        .{ .op = .add, .operands = &.{}, .expected = &.{@intFromEnum(Opcode.add)} },
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

test "Instructions String" {
    const instructions: []const Instructions = &.{
        try makeBytecode(talloc, .add, &.{}),
        try makeBytecode(talloc, .constant, &.{2}),
        try makeBytecode(talloc, .constant, &.{65535}),
    };
    defer for (instructions) |ins| talloc.free(ins);

    const expected =
        \\0000 add
        \\0001 constant 2
        \\0004 constant 65535
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
