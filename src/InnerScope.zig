const std = @import("std");
const code = @import("code.zig");
const Compiler = @import("Compiler.zig");

const Scope = @This();
instructions: *std.ArrayList(code.Instructions),
last_ins: EmittedInstruction,
prev_ins: EmittedInstruction,

pub const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: usize,
};

test "Compiler Scopes" {
    const talloc = std.testing.allocator;
    var compiler: Compiler = try .init(talloc);
    defer compiler.deinit();

    if (compiler.scope_index != 0) {
        std.log.err("Expect 0, found {}", .{compiler.scope_index});
        return error.WrongScopeIndex;
    }

    const global_symbols = compiler.symbols;

    try compiler.emit(.mul, &.{});

    try compiler.enterScope();
    if (compiler.scope_index != 1) {
        std.log.err("Expect 1, found {}", .{compiler.scope_index});
        return error.WrongScopeIndex;
    }

    try compiler.emit(.sub, &.{});

    if (compiler.scopes.items[compiler.scope_index].instructions.items.len != 1) {
        std.log.err("Expect 1, found {}", .{compiler.scopes.items[compiler.scope_index].instructions.items.len});
        return error.WrongInstructionLen;
    }

    if (compiler.scopes.items[compiler.scope_index].last_ins.opcode != .sub) {
        return error.WrongOpcode;
    }

    if (!std.meta.eql(compiler.symbols.?.outer, global_symbols)) {
        return error.WrongEnclosedSymbols;
    }

    const ins = try compiler.leaveScope();
    compiler.allocator.free(ins);
    if (compiler.scope_index != 0) {
        return error.WrongScopeIndex;
    }

    if (!std.meta.eql(compiler.symbols, global_symbols)) {
        return error.WrongRestoringEnclosedSymbols;
    }

    if (compiler.symbols.?.outer != null) {
        return error.IncorrectlyGlobalSymboltable;
    }

    try compiler.emit(.add, &.{});

    if (compiler.scopes.items[compiler.scope_index].instructions.items.len != 2) {
        std.log.err("Expect 2, found {}", .{compiler.scopes.items[compiler.scope_index].instructions.items.len});
        return error.WrongInstructionLen;
    }

    if (compiler.scopes.items[compiler.scope_index].last_ins.opcode != .add) {
        return error.WrongOpcode;
    }

    if (compiler.scopes.items[compiler.scope_index].prev_ins.opcode != .mul) {
        return error.WrongOpcode;
    }
}
