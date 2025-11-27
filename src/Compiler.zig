/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();

const std = @import("std");
const ast = @import("ast.zig");
const code = @import("code.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const SymbolTable = @import("SymbolTable.zig");
const Scope = @import("Scope.zig");
const builtins = @import("builtins.zig");
const Line = @import("Line.zig");
const Error = @import("Error.zig");
const Allocator = std.mem.Allocator;

errors: *Error,
/// constants pool
constants: std.ArrayList(Value) = .empty,
/// token positions
positions: std.ArrayList(usize) = .empty,
scopes: std.ArrayList(Scope) = .empty,
symbols: ?*SymbolTable,
scope_index: usize,
block_index: usize = 0,
/// TODO: labels
loop: Loop = .{},
/// public declaration names
imports: std.StringHashMapUnmanaged(void),
struct_fields: std.ArrayList(struct { index: usize, fields: [][]const u8 = &.{} }) = .empty,

const Loop = struct {
    info: [1]struct {
        /// continue position
        start: usize = 0,
        /// break position
        end: ?usize = null,
    } = .{.{}},
    idx: usize = 0,
};

fn newError(c: *Compiler, gpa: Allocator, comptime fmt: []const u8, args: anytype) anyerror {
    const pos = c.positions.getLast();
    const line: Line = .init(c.errors.input, pos);
    try c.errors.append(gpa, Error.BOLD ++ "{s}:{}:{}: ", .{ c.errors.file, line.start, line.index });
    try c.errors.append(gpa, Error.RED ++ "Compilation Error: " ++ Error.END ++ fmt ++ "\n", args);
    try c.errors.append(gpa, "\t{s}\n\t", .{line.line});
    // try c.errors.msg.writer().writeByteNTimes(' ', line.start);
    try c.errors.append(gpa, "\x1b[32m^\x1b[0m\n", .{});
    return error.Compilation;
}

pub fn init(gpa: Allocator, errors: *Error) !Compiler {
    const main_scope: Scope = .{
        .instructions = .{},
        .last_ins = undefined,
        .prev_ins = undefined,
    };

    var scopes: std.ArrayList(Scope) = .{};
    try scopes.append(gpa, main_scope);
    errdefer scopes.deinit(gpa);

    const s: *SymbolTable = try .create(gpa);
    errdefer gpa.destroy(s);

    return .{
        .symbols = s,
        .scope_index = 0,
        .errors = errors,
        .scopes = scopes,
        .imports = .empty,
        .struct_fields = .empty,
    };
}

pub fn deinit(c: *Compiler, gpa: Allocator) void {
    for (c.scopes.items) |*scope| {
        for (scope.instructions.items) |ins| gpa.free(ins);
        // TODO
        // @constCast(scope.instructions).deinit(gpa);
    }

    c.scopes.deinit(gpa);
    c.constants.deinit(gpa);
    // GC
    if (c.symbols) |s| {
        s.deinit(gpa);
        gpa.destroy(s);
    }
    c.positions.deinit(gpa);
    c.imports.deinit(gpa);
}

fn loadSymbol(c: *Compiler, gpa: Allocator, s: *SymbolTable.Symbol) !void {
    switch (s.scope) {
        .local => try c.emit(gpa, .getlv, &.{s.index}),
        .global => try c.emit(gpa, .getgv, &.{s.index}),
        .builtin => try c.emit(gpa, .getbf, &.{s.index}),
        .free => try c.emit(gpa, .getfree, &.{s.index}),
        .function => try c.emit(gpa, .current_closure, &.{}),
    }
}

/// TODO: make instructions a pointer
pub fn enterScope(c: *Compiler, gpa: Allocator) !void {
    try c.scopes.append(gpa, .{
        .prev_ins = undefined,
        .last_ins = undefined,
        .instructions = .{},
    });
    c.scope_index += 1;
    c.symbols = if (c.symbols) |s| try s.initEnclosed(gpa) else null;
}

pub fn leaveScope(c: *Compiler, gpa: Allocator) !code.Instructions {
    c.scope_index -= 1;
    var last_scope = c.scopes.pop() orelse unreachable;
    defer {
        for (last_scope.instructions.items) |ins| gpa.free(ins);
        last_scope.instructions.deinit(gpa);
    }

    c.symbols = if (c.symbols) |s| b: {
        defer s.deinitEnclosed(gpa);
        break :b s.outer;
    } else null;

    return try std.mem.concat(gpa, u8, last_scope.instructions.items);
}

fn emitFunction(c: *Compiler, gpa: Allocator, func_stmt: *const ast.FunctionStatement) anyerror!void {
    const name = func_stmt.name.value;
    if (c.symbols.?.store.contains(name)) {
        return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
    }
    const symbol = try c.symbols.?.define(gpa, name);

    const func = func_stmt.func;

    for (func.parameters) |parameter| {
        if (c.symbols.?.store.contains(parameter.value)) {
            return c.newError(gpa, "Redeclaration of function parameter '{s}'", .{parameter.value});
        }
    }

    try c.enterScope(gpa);

    // integer overflow! With the fallowing line commented,
    // the internal function error like UndefinedVariable is returned
    // errdefer if (c.symbols) |s| gpa.destroy(s);
    // but leaks memory

    for (func.parameters) |parameter| {
        _ = try c.symbols.?.define(gpa, parameter.value);
    }

    try c.compile(gpa, .{ .statement = .{ .block = func.body } });

    // return the last value if no return statement
    if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn(gpa);

    // return null
    if (!c.lastInstructionIs(.retv)) try c.emit(gpa, .retn, &.{});

    const obj = try gpa.create(Object);
    errdefer gpa.destroy(obj);

    const num_locals = if (c.symbols) |s| s.def_number else 0;

    const instructions = try c.leaveScope(gpa);
    errdefer gpa.free(instructions);

    obj.type = .{
        .function = .{
            // TODO
            .name = func_stmt.name.value,
            .instructions = instructions,
            .num_locals = num_locals,
            .num_parameters = func.parameters.len,
        },
    };

    const pos = try c.addConstants(gpa, .{ .obj = obj });

    try c.emit(gpa, .constant, &.{pos});
    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    try c.emit(gpa, op, &.{symbol.index});
}

fn emitdecl(c: *Compiler, gpa: Allocator, func_stmt: *const ast.FunctionStatement) anyerror!usize {
    // const symbol = try c.symbols.?.define(gpa, func_stmt.name.value);
    const func = func_stmt.func;

    try c.enterScope(gpa);

    // integer overflow! With the fallowing line commented,
    // the internal function error like UndefinedVariable is returned
    // errdefer if (c.symbols) |s| gpa.destroy(s);
    // but leaks memory

    for (func.parameters) |parameter| {
        _ = try c.symbols.?.define(gpa, parameter.value);
    }

    try c.compile(gpa, .{ .statement = .{ .block = func.body } });

    // return the last value if no return statement
    if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn(gpa);

    // return null
    if (!c.lastInstructionIs(.retv)) try c.emit(gpa, .retn, &.{});

    const obj = try gpa.create(Object);
    errdefer gpa.destroy(obj);

    const num_locals = if (c.symbols) |s| s.def_number else 0;

    const instructions = try c.leaveScope(gpa);
    errdefer gpa.free(instructions);

    obj.type = .{
        .decl = .{
            .name = func_stmt.name.value,
            .instructions = instructions,
            .num_locals = num_locals,
            .num_parameters = func.parameters.len,
        },
    };

    const pos = try c.addConstants(gpa, .{ .obj = obj });
    try c.emit(gpa, .constant, &.{pos});
    return pos;
    // const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    // try c.emit(gpa, op, &.{symbol.index});
}

/// Walks the AST recursively and evaluate the node, and add it the the pool
pub fn compile(c: *Compiler, gpa: Allocator, node: ast.Node) !void {
    try c.positions.append(gpa, node.position());
    switch (node) {
        .statement => |stmt| switch (stmt) {
            .program => |program| {
                for (program.statements.items) |s| {
                    try c.compile(gpa, .{ .statement = s });
                }
            },

            .exp_statement => |exp| {
                try c.compile(gpa, .{ .expression = exp.expression });
                try c.emit(gpa, .pop, &.{});
            },

            .block => |block| {
                c.block_index += 1;
                for (block.statements) |blk_stmt| {
                    try c.compile(gpa, .{ .statement = blk_stmt });
                }
                c.block_index -= 1;
            },

            .@"pub" => |p| {
                try c.compile(gpa, .{ .statement = p.stmt.* });

                switch (p.stmt.*) {
                    inline .@"var", .@"fn", .import => |v| {
                        try c.imports.put(gpa, v.name.value, {});
                    },
                    else => return c.newError(gpa, "Invalid public declaration", .{}),
                }
            },

            // this is a bad implementation
            // lazy import are better; this means we can resolve the missing symbols at compile time,
            // However there is no need to compile every statement!
            .import => |imp| {
                const name = imp.name.value;
                const symbol = try c.symbols.?.define(gpa, name);

                // old state
                const cst = c.symbols;

                // new state
                const st = try SymbolTable.create(gpa);
                c.symbols = st;
                st.def_number = cst.?.def_number + 1;

                // TODO: don't compile all, just the global imports and free var
                try c.compile(gpa, imp.node.*);

                // Ay papy, such a poooor logic
                var iter = st.store.iterator();
                while (iter.next()) |item| {
                    const nombre = item.key_ptr.*;
                    if (!c.imports.contains(nombre)) continue;
                    item.value_ptr.public = true;
                }
                c.imports.clearAndFree(gpa);

                // row back
                cst.?.def_number = st.def_number + 1;
                c.symbols = cst;

                const obj = try gpa.create(Object);
                errdefer gpa.destroy(obj);
                obj.type = .{
                    .namespace = .{
                        .name = name,
                        .map = st,
                    },
                };
                const pos = try c.addConstants(gpa, .{ .obj = obj });
                try c.emit(gpa, .constant, &.{pos});

                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(gpa, op, &.{symbol.index});
            },

            .@"var" => |var_stmt| {
                const name = var_stmt.name.value;
                if (c.symbols.?.store.contains(name)) {
                    return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
                }
                const symbol = try c.symbols.?.define(gpa, name);

                if (var_stmt.value.* == .type) {
                    var_stmt.value.type.name = name;
                }

                try c.compile(gpa, .{ .expression = var_stmt.value });
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(gpa, op, &.{symbol.index});
            },

            // TODO: allow immutable data
            .con => |con_stmt| {
                if (con_stmt.name.len > 1 and con_stmt.value.* != .array) {
                    return c.newError(gpa, "Invalid Constant Assignment", .{});
                }

                // const name = con_stmt.name.value;
                var symbols = try gpa.alloc(SymbolTable.Symbol, con_stmt.name.len);
                defer gpa.free(symbols);

                for (con_stmt.name, 0..) |name, i| {
                    if (c.symbols.?.store.contains(name.value)) {
                        return c.newError(gpa, "Redeclaration of variable '{s}'", .{name.value});
                    }
                    symbols[i] = try c.symbols.?.define(gpa, name.value);
                    if (con_stmt.value.* == .type) {
                        con_stmt.value.type.name = name.value;
                    }
                }

                if (symbols.len == 1) {
                    const symbol = symbols[0];
                    try c.compile(gpa, .{ .expression = con_stmt.value });
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(gpa, op, &.{symbol.index});
                    return;
                }

                if (symbols.len != con_stmt.value.array.elements.len) {
                    return c.newError(gpa, "Assignment mismatch", .{});
                }

                for (symbols, con_stmt.value.array.elements) |*symbol, value| {
                    symbol.con = true;
                    try c.compile(gpa, .{ .expression = value });
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(gpa, op, &.{symbol.index});
                }
            },

            .@"return" => |ret| {
                if (c.scope_index == 0) {
                    return c.newError(gpa, "Invalid Return Statement Outside Function Body", .{});
                }

                try c.compile(gpa, .{ .expression = ret.value });
                try c.emit(gpa, .retv, &.{});
            },

            // TODO: make it work in if/match/for
            .@"break" => |ret| {
                if (c.block_index == 0) {
                    return c.newError(gpa, "Invalid break Statement Outside Body", .{});
                }
                try c.compile(gpa, .{ .expression = ret.value });
                const pos = try c.emitPos(gpa, .jump, &.{9999});
                c.loop.info[c.loop.idx].end = pos;
            },

            .@"continue" => |ret| {
                if (c.block_index == 0) {
                    return c.newError(gpa, "Invalid continue Statement Outside Body", .{});
                }
                try c.compile(gpa, .{ .expression = ret.value });
                try c.emit(gpa, .jump, &.{c.loop.info[c.loop.idx].start});
            },

            .@"fn" => |func_stmt| {
                try c.emitFunction(gpa, &func_stmt);
            },

            else => {
                return c.newError(gpa, "Invalid Statement", .{});
            },
        },

        .expression => |exp| switch (exp.*) {
            .identifier => |ident| {
                const symbol = try c.symbols.?.resolve(gpa, ident.value) orelse {
                    return c.newError(gpa, "Undefined Variable '{s}'", .{ident.value});
                };
                try c.loadSymbol(gpa, symbol);
            },

            .group => |goup| {
                try c.compile(gpa, .{ .expression = goup.exp });
            },

            // TODO: not fully implemented
            .assignment => |assignment| {
                if (assignment.operator == .@":=") {
                    if (assignment.name.* == .identifier) {
                        const name = assignment.name.identifier.value;
                        if (c.symbols.?.store.contains(name)) {
                            return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
                        }
                        const symbol = try c.symbols.?.define(gpa, name);

                        if (assignment.value.* == .type) {
                            assignment.value.type.name = name;
                        }

                        try c.compile(gpa, .{ .expression = assignment.value });
                        const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                        try c.emit(gpa, op, &.{symbol.index});
                        try c.emit(gpa, .null, &.{});
                        return;
                    }

                    if (assignment.name.* == .tuple) {
                        const elements = assignment.name.tuple.elements;
                        var op: code.Opcode = .setgv;
                        for (elements) |element| {
                            const name = element.identifier.value;
                            if (c.symbols.?.store.contains(name)) {
                                return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
                            }
                            const symbol = try c.symbols.?.define(gpa, name);

                            if (assignment.value.* == .type) {
                                assignment.value.type.name = name;
                            }

                            try c.emit(gpa, .null, &.{});
                            op = if (symbol.scope == .global) .setgv else .setlv;
                            try c.emit(gpa, op, &.{symbol.index});
                        }

                        try c.compile(gpa, .{ .expression = assignment.value });
                        try c.emit(gpa, .destruct, &.{
                            elements.len,
                            @intFromEnum(op),
                            c.symbols.?.def_number,
                        });

                        return;
                    }
                }

                if (assignment.name.* == .tuple and assignment.operator == .@"=") {
                    const elements = assignment.name.tuple.elements;
                    var op: code.Opcode = .setgv;
                    for (elements) |element| {
                        const name = element.identifier.value;
                        if (!c.symbols.?.store.contains(name)) {
                            return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
                        }
                        const symbol = try c.symbols.?.define(gpa, name);

                        if (assignment.value.* == .type) {
                            assignment.value.type.name = name;
                        }

                        try c.emit(gpa, .null, &.{});
                        op = if (symbol.scope == .global) .setgv else .setlv;
                        try c.emit(gpa, op, &.{symbol.index});
                    }

                    try c.compile(gpa, .{ .expression = assignment.value });
                    try c.emit(gpa, .destruct, &.{
                        elements.len,
                        @intFromEnum(op),
                        c.symbols.?.def_number,
                    });

                    return;
                }

                if (assignment.name.* == .identifier) {
                    const symbol = try c.symbols.?.resolve(gpa, assignment.name.identifier.value) orelse {
                        return c.newError(gpa, "Undefined Variable", .{});
                    };

                    switch (assignment.operator) {
                        .@"=" => {
                            try c.compile(gpa, .{ .expression = assignment.value });
                        },

                        .@"+=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = assignment.value });
                            try c.emit(gpa, .add, &.{});
                        },

                        .@"-=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = assignment.value });
                            try c.emit(gpa, .sub, &.{});
                        },

                        .@"*=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = assignment.value });
                            try c.emit(gpa, .mul, &.{});
                        },

                        .@"/=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = assignment.value });
                            try c.emit(gpa, .div, &.{});
                        },

                        else => return c.newError(gpa, "Invalid Assignment Operator", .{}),
                    }

                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(gpa, op, &.{symbol.index}); // sapporra emite um pop
                    try c.emit(gpa, .null, &.{}); // will be pop, no integer overflow. why?
                    return;
                }

                if (assignment.name.* == .index) {
                    const left = assignment.name.index.left;
                    const index = assignment.name.index.index;
                    const value = assignment.value;

                    try c.compile(gpa, .{ .expression = left });
                    try c.compile(gpa, .{ .expression = index });
                    switch (assignment.operator) {
                        .@"=" => {
                            try c.compile(gpa, .{ .expression = value });
                        },

                        .@"+=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .add, &.{});
                        },
                        .@"-=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .sub, &.{});
                        },

                        .@"*=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .mul, &.{});
                        },

                        .@"/=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .div, &.{});
                        },

                        else => return c.newError(gpa, "Invalid Assignment Operator", .{}),
                    }

                    return try c.emit(gpa, .index_set, &.{});
                }

                if (assignment.name.* == .method) {
                    const left = assignment.name.method.caller;
                    const method: Value = .{ .tag = assignment.name.method.method.value };
                    const value = assignment.value;

                    try c.compile(gpa, .{ .expression = left });
                    const pos = try c.addConstants(gpa, method);

                    switch (assignment.operator) {
                        .@"=" => {
                            try c.compile(gpa, .{ .expression = value });
                        },
                        .@"+=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .add, &.{});
                        },

                        .@"-=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .sub, &.{});
                        },

                        .@"*=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .mul, &.{});
                        },

                        .@"/=" => {
                            try c.compile(gpa, .{ .expression = assignment.name });
                            try c.compile(gpa, .{ .expression = value });
                            try c.emit(gpa, .div, &.{});
                        },

                        else => return c.newError(gpa, "Invalid Assignment Operator", .{}),
                    }

                    try c.emit(gpa, .method_set, &.{pos});
                    return;
                }

                return c.newError(gpa, "Invalid Assignment Operator", .{});
            },

            .infix => |infix| {
                const operator = infix.operator;

                if (operator == .@"<") {
                    try c.compile(gpa, .{ .expression = infix.right });
                    try c.compile(gpa, .{ .expression = infix.left });
                    try c.emit(gpa, .gt, &.{});
                    return;
                }

                if (operator == .@"<=") {
                    try c.compile(gpa, .{ .expression = infix.right });
                    try c.compile(gpa, .{ .expression = infix.left });
                    try c.emit(gpa, .gte, &.{});
                    return;
                }

                try c.compile(gpa, .{ .expression = infix.left });
                try c.compile(gpa, .{ .expression = infix.right });
                const op: code.Opcode = switch (operator) {
                    .@"and" => .land,
                    .@"or" => .lor,
                    .@"+" => .add,
                    .@"-" => .sub,
                    .@"*" => .mul,
                    .@"^" => .pow,
                    .@"/" => .div,
                    .@">" => .gt,
                    .@">=" => .gte,
                    .@"==" => .eq,
                    .@"!=" => .neq,
                    .@"%" => .mod,
                    else => return c.newError(gpa, "Unkow Operator", .{}),
                };
                try c.emit(gpa, op, &.{});
            },

            .prefix => |prefix| {
                const operator = prefix.operator;
                try c.compile(gpa, .{ .expression = prefix.right });
                switch (operator) {
                    .@"!" => try c.emit(gpa, .not, &.{}),
                    .@"-" => try c.emit(gpa, .min, &.{}),
                    else => unreachable,
                }
            },

            .index => |index| {
                try c.compile(gpa, .{ .expression = index.left });
                try c.compile(gpa, .{ .expression = index.index });
                try c.emit(gpa, .index_get, &.{});
            },

            // TODO: rework
            .method => |method| {
                try c.compile(gpa, .{ .expression = method.caller });
                const pos = try c.addConstants(gpa, .{
                    .tag = method.method.value,
                });
                try c.emit(gpa, .constant, &.{pos});
                try c.emit(gpa, .index_get, &.{});
                return;
            },

            .boolean => |boolean| {
                const op: code.Opcode = if (boolean.value) .true else .false;
                try c.emit(gpa, op, &.{});
            },

            .null => {
                try c.emit(gpa, .null, &.{});
            },

            // TODO: think !!
            .range => |range| {
                try c.compile(gpa, .{ .expression = range.end });
                try c.compile(gpa, .{ .expression = range.start });
                try c.emit(gpa, .set_range, &.{});
            },

            .integer => |int| {
                const pos = try c.addConstants(gpa, .{
                    .integer = int.value,
                });
                try c.emit(gpa, .constant, &.{pos});
            },

            .tag => |tag| {
                const pos = try c.addConstants(gpa, .{
                    .tag = tag.value,
                    //.from = tag.from,
                });
                try c.emit(gpa, .constant, &.{pos});
            },

            .char => |char| {
                const pos = try c.addConstants(gpa, .{
                    .char = char.value,
                });
                try c.emit(gpa, .constant, &.{pos});
            },

            .float => |float| {
                const pos = try c.addConstants(gpa, .{
                    .float = float.value,
                });
                try c.emit(gpa, .constant, &.{pos});
            },

            .string => |str| {
                // const cvalue = try gpa.dupe(u8, str.value);
                // errdefer gpa.free(cvalue);
                var cvalue = try std.ArrayList(u8).initCapacity(gpa, str.value.len);
                errdefer cvalue.deinit(gpa);
                try cvalue.appendSlice(gpa, str.value);

                const obj = try gpa.create(Object);
                errdefer gpa.destroy(obj);
                obj.type = .{ .string = cvalue };

                const pos = try c.addConstants(gpa, .{ .obj = obj });
                try c.emit(gpa, .constant, &.{pos});
            },

            inline .array, .tuple => |array| {
                for (array.elements) |element| {
                    try c.compile(gpa, .{ .expression = element });
                }
                try c.emit(gpa, .array, &.{array.elements.len});
            },

            .hash => |hash| {
                for (hash.pairs) |pair| {
                    const key, const val = pair;
                    try c.compile(gpa, .{ .expression = key });
                    try c.compile(gpa, .{ .expression = val });
                }
                try c.emit(gpa, .hash, &.{hash.pairs.len * 2});
            },

            .call => |call| {
                try c.compile(gpa, .{ .expression = call.function });
                for (call.arguments) |arg| {
                    try c.compile(gpa, .{ .expression = arg });
                }
                try c.emit(gpa, .call, &.{call.arguments.len});
            },

            .instance => |instance| {
                try c.compile(gpa, .{ .expression = instance.type });
                for (instance.fields) |field| {
                    const ident = field.name;
                    const pos = try c.addConstants(gpa, .{ .tag = ident.value });
                    try c.emit(gpa, .constant, &.{pos});
                    try c.compile(gpa, .{ .expression = field.value });
                }
                try c.emit(gpa, .instance, &.{instance.fields.len});
            },

            .function => |func| {
                try c.enterScope(gpa);

                // TODO: ast
                if (func.name) |name| {
                    _ = try c.symbols.?.defineFunctionName(gpa, name.value);
                }

                for (func.parameters) |parameter| {
                    _ = try c.symbols.?.define(gpa, parameter.value);
                }

                try c.compile(gpa, .{ .statement = .{ .block = func.body } });

                // return the last value if no return statement
                if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn(gpa);

                // return null
                if (!c.lastInstructionIs(.retv)) try c.emit(gpa, .retn, &.{});

                const free_symbols = c.symbols.?.frees;
                const obj = try gpa.create(Object);
                errdefer gpa.destroy(obj);
                const num_locals = if (c.symbols) |s| s.def_number else 0;

                for (free_symbols.items) |symb| {
                    try c.loadSymbol(gpa, symb);
                }
                const num_free_symbols = free_symbols.items.len;

                const instructions = try c.leaveScope(gpa);
                errdefer gpa.free(instructions);

                obj.type = .{ .closure = .{
                    .func = .{
                        .instructions = instructions,
                        .num_locals = num_locals,
                        .num_parameters = func.parameters.len,
                    },
                } };

                const pos = try c.addConstants(gpa, .{ .obj = obj });
                try c.emit(gpa, .closure, &.{ pos, num_free_symbols });
            },

            .@"if" => |ifexp| {
                try c.compile(gpa, .{ .expression = ifexp.condition });

                // since a new scope will be created, we need the counter
                // const len = c.insLen();
                const jump_if_not_true_pos = try c.emitPos(gpa, .jumpifnottrue, &.{9999});

                if (ifexp.consequence.statements.len == 0) {
                    try c.emit(gpa, .null, &.{});
                } else {
                    // compiling the consequence
                    try c.compile(gpa, .{ .statement = .{ .block = ifexp.consequence } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
                }

                // always jump: null is returned
                const jump_pos = try c.emitPos(gpa, .jump, &.{9999});
                const after_consequence_position = c.insLen();

                // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
                try c.changeOperand(gpa, jump_if_not_true_pos, after_consequence_position);

                if (ifexp.alternative) |alt| {
                    if (alt.statements.len == 0) {
                        try c.emit(gpa, .null, &.{});
                    } else {
                        try c.compile(gpa, .{ .statement = .{ .block = alt } });
                        // statements add a pop in the end, wee drop the last pop (if return)
                        if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
                    }
                } else {
                    try c.emit(gpa, .null, &.{});
                }

                const after_alternative_pos = c.insLen();
                // Replaces the 9999 (.jump) to the correct operand (after_alternative_pos);
                try c.changeOperand(gpa, jump_pos, after_alternative_pos);
            },

            .match => |match| {
                var jumps_pos_list: std.ArrayList(usize) = .empty;
                defer jumps_pos_list.deinit(gpa);

                // try c.enterScope(gpa);
                for (match.arms) |arm| for (arm.condition) |condition| {
                    try c.compile(gpa, .{ .expression = match.value });
                    try c.compile(gpa, .{ .expression = condition });
                    try c.emit(gpa, .eq, &.{});

                    const jump_if_not_true_pos = try c.emitPos(gpa, .jumpifnottrue, &.{9999});

                    if (arm.block.statements.len == 0) {
                        try c.emit(gpa, .null, &.{});
                    } else {
                        try c.compile(gpa, .{ .statement = .{ .block = arm.block } });
                        if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
                    }

                    const jum_pos = try c.emitPos(gpa, .jump, &.{9999});
                    try jumps_pos_list.append(gpa, jum_pos);

                    const after_consequence_position = c.insLen();
                    try c.changeOperand(gpa, jump_if_not_true_pos, after_consequence_position);
                };

                if (match.else_block) |block| {
                    if (block.statements.len == 0) {
                        try c.emit(gpa, .null, &.{});
                    } else {
                        try c.compile(gpa, .{ .statement = .{ .block = block } });
                        if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
                    }
                } else {
                    try c.emit(gpa, .null, &.{});
                }

                const after_alternative_pos = c.insLen();

                for (jumps_pos_list.items) |jum_pos| {
                    try c.changeOperand(gpa, jum_pos, after_alternative_pos);
                }
            },

            .@"for" => |forloop| {
                const jump_pos = c.insLen();
                c.loop.info[c.loop.idx].start = jump_pos;

                try c.compile(gpa, .{ .expression = forloop.condition });

                // fake instruction position
                const jump_if_not_true_pos = try c.emitPos(gpa, .jumpifnottrue, &.{9999});

                // compiling the consequence
                if (forloop.consequence.statements.len == 0) {
                    try c.emit(gpa, .null, &.{});
                } else {
                    try c.compile(gpa, .{ .statement = .{ .block = forloop.consequence } });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
                }

                try c.emit(gpa, .jump, &.{jump_pos});

                // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
                // const after_consequence_position = c.insLen() + start_pos;
                const after_consequence_position = c.insLen();
                try c.changeOperand(gpa, jump_if_not_true_pos, after_consequence_position);

                // // break statement
                if (c.loop.info[c.loop.idx].end) |pox| {
                    try c.changeOperand(gpa, pox, after_consequence_position);
                    c.loop.info[c.loop.idx].end = null;
                } else {
                    // // always jump: null is returned
                    try c.emit(gpa, .null, &.{});
                }
            },

            .for_range => |*forloop| {
                try if (forloop.index == null) c.emitLoop(gpa, forloop) else c.emitLoopWithIndex(gpa, forloop);
            },

            // TODO: make it like a function
            //
            // a type must have his own constants and variables list
            // this will make modules/import more easy!
            //
            // * VM will have a new field called: types *
            //
            // in this case, methods call will have the callee passed as arguments (always)
            .type => |ty| {
                const fields = ty.fields;
                const decls = ty.decl;

                const e: Object.BuiltinType.BT = if (ty.type == .@"struct") .@"struct" else .@"enum";

                const pos_name = try c.addConstants(gpa, .{
                    .tag = ty.name orelse "struct",
                });
                try c.emit(gpa, .constant, &.{pos_name});

                for (fields) |field| {
                    const ident = field.name;
                    const value = field.value;

                    const pos = try c.addConstants(gpa, .{ .tag = ident.value });
                    try c.emit(gpa, .constant, &.{pos});

                    try c.compile(gpa, .{ .expression = value });
                }

                for (decls) |dec| {
                    // function name as a field
                    const pos = try c.addConstants(gpa, .{ .tag = dec.name.value });
                    try c.emit(gpa, .constant, &.{pos});
                    _ = try c.emitdecl(gpa, &dec);
                }

                // NOTE: will work?
                // const pos = c.addConstants(gpa,.{ .@"struct" = .{
                //     .fields_len = fields.len,
                //     .decl_len = decls.len,
                //     .name = name
                //     .instructions = ...
                // } })

                try c.emit(gpa, .type, &.{ decls.len + 1, fields.len, @intFromEnum(e) });
            },

            else => |exx| {
                return c.newError(gpa, "Invalid Expression '{}'", .{exx});
            },
        },
    }
}

fn define(c: *Compiler, gpa: Allocator, name: []const u8) !void {
    if (c.symbols.?.store.contains(name)) return c.newError(gpa, "Redeclaration of variable '{s}'", .{name});
    const symbol = try c.symbols.?.define(gpa, name);
    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    try c.emit(gpa, op, &.{symbol.index});
}

fn emitLoopWithIndex(c: *Compiler, gpa: Allocator, forloop: *ast.ForRange) anyerror!void {
    // cast the expression as a range and load it as a constant
    const name = forloop.ident;
    const iname = forloop.index.?;
    defer {
        _ = c.symbols.?.store.swapRemove(name);
        _ = c.symbols.?.store.swapRemove(iname);
    }
    // TODO: dont do this shit!!!!
    const pos = try c.addConstants(gpa, .null);
    const ipos = try c.addConstants(gpa, .null);
    try c.emit(gpa, .constant, &.{ipos});
    try c.emit(gpa, .constant, &.{pos});
    try c.compile(gpa, .{ .expression = forloop.iterable });
    try c.emit(gpa, .to_range, &.{pos});

    const jump_pos = c.insLen();
    c.loop.info[c.loop.idx].start = jump_pos;
    // in this block, the last instruction must be boolean
    {
        try c.emit(gpa, .get_range_idx, &.{pos});
    }

    const jump_if_not_true_pos = try c.emitPos(gpa, .jumpifnottrue, &.{9999});

    // compiling the consequence
    if (forloop.body.statements.len != 0) {
        // item
        try c.define(gpa, name);
        try c.define(gpa, iname);

        try c.compile(gpa, .{ .statement = .{ .block = forloop.body } });
        // statements add a pop in the end, wee drop the last pop (if return)
        if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
    }

    try c.emit(gpa, .jump, &.{jump_pos});

    // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
    const after_consequence_position = c.insLen();

    // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
    try c.changeOperand(gpa, jump_if_not_true_pos, after_consequence_position);

    // // break statement
    if (c.loop.info[c.loop.idx].end) |pox| {
        try c.changeOperand(gpa, pox, after_consequence_position);
        c.loop.info[c.loop.idx].end = null;
    } else {
        // // always jump: null is returned
        try c.emit(gpa, .null, &.{});
    }
}

fn emitLoop(c: *Compiler, gpa: Allocator, forloop: *ast.ForRange) anyerror!void {
    // cast the expression as a range and load it as a constant
    const name = forloop.ident;
    defer _ = c.symbols.?.store.swapRemove(name);

    // TODO: dont do this shit!!!!
    const pos = try c.addConstants(gpa, .null);
    try c.emit(gpa, .constant, &.{pos});
    try c.compile(gpa, .{ .expression = forloop.iterable });
    try c.emit(gpa, .to_range, &.{pos});

    const jump_pos = c.insLen();
    c.loop.info[c.loop.idx].start = jump_pos;
    // in this block, the last instruction must be boolean
    {
        try c.emit(gpa, .get_range, &.{pos});
    }

    const jump_if_not_true_pos = try c.emitPos(gpa, .jumpifnottrue, &.{9999});

    // compiling the consequence
    if (forloop.body.statements.len != 0) {
        try c.define(gpa, name);

        try c.compile(gpa, .{ .statement = .{ .block = forloop.body } });
        // statements add a pop in the end, wee drop the last pop (if return)
        if (c.lastInstructionIs(.pop)) c.removeLastPop(gpa);
    }

    try c.emit(gpa, .jump, &.{jump_pos});

    // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
    const after_consequence_position = c.insLen();

    // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
    try c.changeOperand(gpa, jump_if_not_true_pos, after_consequence_position);

    // // break statement
    if (c.loop.info[c.loop.idx].end) |pox| {
        try c.changeOperand(gpa, pox, after_consequence_position);
        c.loop.info[c.loop.idx].end = null;
    } else {
        // // always jump: null is returned
        try c.emit(gpa, .null, &.{});
    }
}

fn insLen(c: *Compiler) usize {
    var len: usize = 0;
    for (c.currentInstruction().items) |ins| len += ins.len;
    return len;
}

fn insLenRec(c: *Compiler) usize {
    var len: usize = 0;
    var si = c.scope_index;
    while (true) {
        for (c.scopes.items[si].instructions.items) |ins| len += ins.len;
        if (si == 0) break;
        si -= 1;
    }
    return len;
}

fn currentInstruction(c: *Compiler) *std.ArrayList(code.Instructions) {
    return &c.scopes.items[c.scope_index].instructions;
}

/// FIX
fn removeLastPop(c: *Compiler, gpa: Allocator) void {
    gpa.free(c.currentInstruction().pop() orelse unreachable);
    c.scopes.items[c.scope_index].last_ins = c.scopes.items[c.scope_index].prev_ins;
}

fn addConstants(c: *Compiler, gpa: Allocator, val: Value) !usize {
    try c.constants.append(gpa, val);
    return c.constants.items.len - 1;
}

fn addInstruction(c: *Compiler, gpa: Allocator, ins: code.Instructions) !usize {
    const pos_new_ins = c.currentInstruction().items.len;
    try c.currentInstruction().append(gpa, ins);
    return pos_new_ins;
}

fn replaceInstruction(c: *Compiler, gpa: Allocator, pos: usize, new_ins: []u8) !void {
    gpa.free(c.currentInstruction().orderedRemove(pos));
    try c.currentInstruction().insert(gpa, pos, new_ins);
}

fn replaceLastPopWithReturn(c: *Compiler, gpa: Allocator) !void {
    const last_pos = c.scopes.items[c.scope_index].last_ins.pos;
    try c.replaceInstruction(gpa, last_pos, try code.makeBytecode(gpa, .retv, &.{}));
    c.scopes.items[c.scope_index].last_ins.opcode = .retv;
}

/// replaces the instruction
fn changeOperand(c: *Compiler, gpa: Allocator, op_pos: usize, operand: usize) !void {
    const op: code.Opcode = @enumFromInt(c.currentInstruction().items[op_pos][0]);
    const new_ins = try code.makeBytecode(gpa, op, &.{operand});
    try c.replaceInstruction(gpa, op_pos, new_ins);
}

/// generate a instruction and add it to a pool
pub fn emit(c: *Compiler, gpa: Allocator, op: code.Opcode, operants: []const usize) !void {
    const ins = try code.makeBytecode(gpa, op, operants);
    const pos = try c.addInstruction(gpa, ins);
    c.setLastInstruction(op, pos);
}

pub fn emitPos(c: *Compiler, gpa: Allocator, op: code.Opcode, operants: []const usize) !usize {
    const ins = try code.makeBytecode(gpa, op, operants);
    const pos = try c.addInstruction(gpa, ins);
    c.setLastInstruction(op, pos);
    return pos;
}

fn setLastInstruction(c: *Compiler, op: code.Opcode, pos: usize) void {
    c.scopes.items[c.scope_index].prev_ins = c.scopes.items[c.scope_index].last_ins;
    c.scopes.items[c.scope_index].last_ins = .{ .opcode = op, .pos = pos };
}

fn lastInstructionIs(c: *Compiler, op: code.Opcode) bool {
    if (c.currentInstruction().items.len == 0) return false;
    return c.scopes.items[c.scope_index].last_ins.opcode == op;
}

pub fn bytecode(c: *Compiler, gpa: Allocator) !Bytecode {
    const ins = try std.mem.concat(gpa, u8, c.currentInstruction().items);
    errdefer gpa.free(ins);
    return .{ .positions = try c.positions.toOwnedSlice(gpa), .constants = c.constants.items, .instructions = ins };
}

fn findImportName(c: *Compiler, gpa: Allocator, path: []const u8) ![]const u8 {
    if (!std.mem.endsWith(u8, path, ".loli")) {
        return c.newError(gpa, "Invalid Module Path: expected .loli extention, got your mama", .{});
    }
    var start = std.mem.lastIndexOf(u8, path, "/") orelse 0;
    if (start != 0) start += 1;
    const end = std.mem.lastIndexOf(u8, path, ".") orelse unreachable;
    return path[start..end];
}

/// compiler generated bytecode
pub const Bytecode = struct {
    constants: []Value,
    positions: []usize,
    instructions: code.Instructions,

    pub fn deinit(b: *Bytecode, gpa: Allocator) void {
        gpa.free(b.instructions);
    }
};

test {
    // _ = @import("compiler_test.zig");
    // _ = SymbolTable;
    // _ = Scope;
}
