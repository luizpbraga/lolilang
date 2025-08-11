const std = @import("std");
const ast = @import("ast.zig");
const code = @import("code.zig");
const Object = @import("Object.zig");
const Value = Object.Value;
const memory = @import("memory.zig");
const SymbolTable = @import("SymbolTable.zig");
const Scope = @import("Scope.zig");
const builtins = @import("builtins.zig");
const Line = @import("Line.zig");
const Error = @import("Error.zig");

/// compiler: transverse the AST, find the ast nodes and evakueate then to objects, and add it to the pool
const Compiler = @This();
errors: *Error,
allocator: std.mem.Allocator,
/// constants pool
constants: std.ArrayList(Value),
/// token positions
positions: std.ArrayList(usize),
symbols: ?*SymbolTable,
scopes: std.ArrayList(Scope),
scope_index: usize,
block_index: usize = 0,
/// TODO: labels
loop: Loop = .{},
/// public declaration names
imports: std.StringHashMap(void),

struct_fields: std.ArrayList(struct { index: usize, fields: [][]const u8 = &.{} }),

const Loop = struct {
    info: [1]struct {
        /// continue position
        start: usize = 0,
        /// break position
        end: ?usize = null,
    } = .{.{}},
    idx: usize = 0,
};

fn newError(c: *Compiler, comptime fmt: []const u8, args: anytype) anyerror {
    const pos = c.positions.getLast();
    const line: Line = .init(c.errors.input, pos);
    try c.errors.msg.writer().print(Error.BOLD ++ "{s}:{}:{}: ", .{ c.errors.file, line.start, line.index });
    try c.errors.msg.writer().print(Error.RED ++ "Compilation Error: " ++ Error.END ++ fmt ++ "\n", args);
    try c.errors.msg.writer().print("\t{s}\n\t", .{line.line});
    try c.errors.msg.writer().writeByteNTimes(' ', line.start);
    try c.errors.msg.writer().writeAll("\x1b[32m^\x1b[0m\n");
    return error.Compilation;
}

pub fn init(alloc: std.mem.Allocator, errors: *Error) !Compiler {
    const main_scope: Scope = .{
        .instructions = .init(alloc),
        .last_ins = undefined,
        .prev_ins = undefined,
    };

    var scopes: std.ArrayList(Scope) = .init(alloc);
    try scopes.append(main_scope);

    const s = try SymbolTable.create(alloc);
    errdefer alloc.destroy(s);

    return .{
        .errors = errors,
        .allocator = alloc,
        .constants = .init(alloc),
        .struct_fields = .init(alloc),
        .positions = .init(alloc),
        .symbols = s,
        .scopes = scopes,
        .scope_index = 0,
        .imports = .init(alloc),
    };
}

pub fn deinit(c: *Compiler) void {
    for (c.scopes.items) |scope| {
        for (scope.instructions.items) |ins| c.allocator.free(ins);
        scope.instructions.deinit();
    }

    c.scopes.deinit();
    c.constants.deinit();
    // GC
    if (c.symbols) |s| {
        s.deinit();
        c.allocator.destroy(s);
    }
    c.positions.deinit();
    c.imports.deinit();
}

fn loadSymbol(c: *Compiler, s: *SymbolTable.Symbol) !void {
    switch (s.scope) {
        .local => try c.emit(.getlv, &.{s.index}),
        .global => try c.emit(.getgv, &.{s.index}),
        .builtin => try c.emit(.getbf, &.{s.index}),
        .free => try c.emit(.getfree, &.{s.index}),
        .function => try c.emit(.current_closure, &.{}),
    }
}

/// TODO: make instructions a pointer
pub fn enterScope(c: *Compiler) !void {
    try c.scopes.append(.{
        .prev_ins = undefined,
        .last_ins = undefined,
        .instructions = .init(c.allocator),
    });
    c.scope_index += 1;
    c.symbols = if (c.symbols) |s| try s.initEnclosed() else null;
}

pub fn leaveScope(c: *Compiler) !code.Instructions {
    c.scope_index -= 1;
    var last_scope = c.scopes.pop() orelse unreachable;
    defer {
        for (last_scope.instructions.items) |ins| c.allocator.free(ins);
        last_scope.instructions.deinit();
    }

    c.symbols = if (c.symbols) |s| b: {
        defer s.deinitEnclosed();
        break :b s.outer;
    } else null;

    return try std.mem.concat(c.allocator, u8, last_scope.instructions.items);
}

fn emitFunction(c: *Compiler, func_stmt: *const ast.FunctionStatement) anyerror!void {
    const name = func_stmt.name.value;
    if (c.symbols.?.store.contains(name)) {
        return c.newError("Redeclaration of variable '{s}'", .{name});
    }
    const symbol = try c.symbols.?.define(name);

    const func = func_stmt.func;

    for (func.parameters) |parameter| {
        if (c.symbols.?.store.contains(parameter.value)) {
            return c.newError("Redeclaration of function parameter '{s}'", .{parameter.value});
        }
    }

    try c.enterScope();

    // integer overflow! With the fallowing line commented,
    // the internal function error like UndefinedVariable is returned
    // errdefer if (c.symbols) |s| c.allocator.destroy(s);
    // but leaks memory

    for (func.parameters) |parameter| {
        _ = try c.symbols.?.define(parameter.value);
    }

    try c.compileStatement(.{ .block = func.body });

    // return the last value if no return statement
    if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

    // return null
    if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

    const obj = try c.allocator.create(Object);
    errdefer c.allocator.destroy(obj);

    const num_locals = if (c.symbols) |s| s.def_number else 0;

    const instructions = try c.leaveScope();
    errdefer c.allocator.free(instructions);

    obj.type = .{
        .function = .{
            // TODO
            .name = func_stmt.name.value,
            .instructions = instructions,
            .num_locals = num_locals,
            .num_parameters = func.parameters.len,
        },
    };

    const pos = try c.addConstants(.{ .obj = obj });

    try c.emit(.constant, &.{pos});
    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    try c.emit(op, &.{symbol.index});
}

fn emitdecl(c: *Compiler, func_stmt: *const ast.FunctionStatement) anyerror!usize {
    // const symbol = try c.symbols.?.define(func_stmt.name.value);
    const func = func_stmt.func;

    try c.enterScope();

    // integer overflow! With the fallowing line commented,
    // the internal function error like UndefinedVariable is returned
    // errdefer if (c.symbols) |s| c.allocator.destroy(s);
    // but leaks memory

    for (func.parameters) |parameter| {
        _ = try c.symbols.?.define(parameter.value);
    }

    try c.compileStatement(.{ .block = func.body });

    // return the last value if no return statement
    if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

    // return null
    if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

    const obj = try c.allocator.create(Object);
    errdefer c.allocator.destroy(obj);

    const num_locals = if (c.symbols) |s| s.def_number else 0;

    const instructions = try c.leaveScope();
    errdefer c.allocator.free(instructions);

    obj.type = .{
        .decl = .{
            .name = func_stmt.name.value,
            .instructions = instructions,
            .num_locals = num_locals,
            .num_parameters = func.parameters.len,
        },
    };

    const pos = try c.addConstants(.{ .obj = obj });
    try c.emit(.constant, &.{pos});
    return pos;
    // const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    // try c.emit(op, &.{symbol.index});
}

/// Walks the AST recursively and evaluate the node, and add it the the pool
pub fn compile(c: *Compiler, stmts: []ast.Statement) anyerror!void {
    for (stmts) |s| try c.compileStatement(s);
}

pub fn compileStatement(c: *Compiler, stmt: ast.Statement) anyerror!void {
    try c.positions.append(stmt.position());
    switch (stmt) {
        .exp_statement => |exp| {
            try c.compileExpression(exp.expression);
            try c.emit(.pop, &.{});
        },

        .block => |block| {
            c.block_index += 1;
            for (block.statements) |blk_stmt| {
                try c.compileStatement(blk_stmt);
            }
            c.block_index -= 1;
        },

        .@"pub" => |p| {
            try c.compileStatement(p.stmt.*);

            switch (p.stmt.*) {
                inline .@"var", .@"fn", .import => |v| {
                    try c.imports.put(v.name.value, {});
                },
                else => return c.newError("Invalid public declaration", .{}),
            }
        },

        // this is a bad implementation
        // lazy import are better; this means we can resolve the missing symbols at compile time,
        // However there is no need to compile every statement!
        .import => |imp| {
            const name = imp.name.value;
            const symbol = try c.symbols.?.define(name);

            // old state
            const cst = c.symbols;

            // new state
            const st = try SymbolTable.create(c.allocator);
            c.symbols = st;
            st.def_number = cst.?.def_number + 1;

            // TODO: don't compile all, just the global imports and free var
            try c.compileStatement(imp.node.*);

            // Ay papy, such a poooor logic
            var iter = st.store.iterator();
            while (iter.next()) |item| {
                const nombre = item.key_ptr.*;
                if (!c.imports.contains(nombre)) continue;
                item.value_ptr.public = true;
            }
            c.imports.clearAndFree();

            // row back
            cst.?.def_number = st.def_number + 1;
            c.symbols = cst;

            const obj = try c.allocator.create(Object);
            errdefer c.allocator.destroy(obj);
            obj.type = .{
                .namespace = .{
                    .name = name,
                    .map = st,
                },
            };
            const pos = try c.addConstants(.{ .obj = obj });
            try c.emit(.constant, &.{pos});

            const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
            try c.emit(op, &.{symbol.index});
        },

        .@"var" => |v| {
            const name = v.name.value;
            if (c.symbols.?.store.contains(name)) {
                return c.newError("Redeclaration of variable '{s}'", .{name});
            }
            const symbol = try c.symbols.?.define(name);

            if (v.value.* == .type) {
                v.value.type.name = name;
            }

            if (v.value.* == .class) {
                v.value.class.name = name;
            }

            try c.compileExpression(v.value);
            const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
            try c.emit(op, &.{symbol.index});
        },

        // TODO: allow immutable data
        .con => |con| {
            if (con.name.len > 1 and con.value.* != .array) {
                return c.newError("Invalid Constant Assignment", .{});
            }

            // const name = con_stmt.name.value;
            var symbols = try c.allocator.alloc(SymbolTable.Symbol, con.name.len);
            defer c.allocator.free(symbols);

            for (con.name, 0..) |name, i| {
                if (c.symbols.?.store.contains(name.value)) {
                    return c.newError("Redeclaration of variable '{s}'", .{name.value});
                }
                symbols[i] = try c.symbols.?.define(name.value);
                if (con.value.* == .type) {
                    con.value.type.name = name.value;
                }
            }

            if (symbols.len == 1) {
                const symbol = symbols[0];
                try c.compileExpression(con.value);
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index});
                return;
            }

            if (symbols.len != con.value.array.elements.len) {
                return c.newError("Assignment mismatch", .{});
            }

            for (symbols, con.value.array.elements) |*symbol, value| {
                symbol.con = true;
                try c.compileExpression(value);
                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index});
            }
        },

        .@"return" => |ret| {
            if (c.scope_index == 0) {
                return c.newError("Invalid Return Statement Outside Function Body", .{});
            }

            try c.compileExpression(ret.value);
            try c.emit(.retv, &.{});
        },

        // TODO: make it work in if/match/for
        .@"break" => |ret| {
            if (c.block_index == 0) {
                return c.newError("Invalid break Statement Outside Body", .{});
            }
            try c.compileExpression(ret.value);
            const pos = try c.emitPos(.jump, &.{9999});
            c.loop.info[c.loop.idx].end = pos;
        },

        .@"continue" => |cont| {
            if (c.block_index == 0) {
                return c.newError("Invalid continue Statement Outside Body", .{});
            }
            try c.compileExpression(cont.value);
            try c.emit(.jump, &.{c.loop.info[c.loop.idx].start});
        },

        .@"fn" => |fun| {
            try c.emitFunction(&fun);
        },

        else => {
            return c.newError("Invalid Statement", .{});
        },
    }
}

fn compileExpression(c: *Compiler, exp: *ast.Expression) anyerror!void {
    switch (exp.*) {
        .identifier => |ident| {
            const symbol = try c.symbols.?.resolve(ident.value) orelse {
                return c.newError("Undefined Variable '{s}'", .{ident.value});
            };
            try c.loadSymbol(symbol);
        },

        .group => |goup| {
            try c.compileExpression(goup.exp);
        },

        // TODO: not fully implemented
        .assignment => |ass| {
            if (ass.operator == .@":=") {
                if (ass.name.* == .identifier) {
                    const name = ass.name.identifier.value;
                    if (c.symbols.?.store.contains(name)) {
                        return c.newError("Redeclaration of variable '{s}'", .{name});
                    }
                    const symbol = try c.symbols.?.define(name);

                    if (ass.value.* == .class) {
                        ass.value.class.name = name;
                    }
                    if (ass.value.* == .type) {
                        ass.value.type.name = name;
                    }

                    try c.compileExpression(ass.value);
                    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                    try c.emit(.null, &.{});
                    return;
                }

                if (ass.name.* == .tuple) {
                    const elements = ass.name.tuple.elements;
                    var op: code.Opcode = .setgv;
                    for (elements) |element| {
                        const name = element.identifier.value;
                        if (c.symbols.?.store.contains(name)) {
                            return c.newError("Redeclaration of variable '{s}'", .{name});
                        }
                        const symbol = try c.symbols.?.define(name);

                        if (ass.value.* == .type) {
                            ass.value.type.name = name;
                        }

                        try c.emit(.null, &.{});
                        op = if (symbol.scope == .global) .setgv else .setlv;
                        try c.emit(op, &.{symbol.index});
                    }

                    try c.compileExpression(ass.value);
                    try c.emit(.destruct, &.{
                        elements.len,
                        @intFromEnum(op),
                        c.symbols.?.def_number,
                    });

                    return;
                }
            }

            if (ass.name.* == .tuple and ass.operator == .@"=") {
                const elements = ass.name.tuple.elements;
                var op: code.Opcode = .setgv;
                for (elements) |element| {
                    const name = element.identifier.value;
                    if (!c.symbols.?.store.contains(name)) {
                        return c.newError("Redeclaration of variable '{s}'", .{name});
                    }
                    const symbol = try c.symbols.?.define(name);

                    if (ass.value.* == .type) {
                        ass.value.type.name = name;
                    }

                    try c.emit(.null, &.{});
                    op = if (symbol.scope == .global) .setgv else .setlv;
                    try c.emit(op, &.{symbol.index});
                }

                try c.compileExpression(ass.value);
                try c.emit(.destruct, &.{
                    elements.len,
                    @intFromEnum(op),
                    c.symbols.?.def_number,
                });

                return;
            }

            if (ass.name.* == .identifier) {
                const symbol = try c.symbols.?.resolve(ass.name.identifier.value) orelse {
                    return c.newError("Undefined Variable", .{});
                };

                switch (ass.operator) {
                    .@"=" => {
                        try c.compileExpression(ass.value);
                    },

                    .@"+=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(ass.value);
                        try c.emit(.add, &.{});
                    },

                    .@"-=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(ass.value);
                        try c.emit(.sub, &.{});
                    },

                    .@"*=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(ass.value);
                        try c.emit(.mul, &.{});
                    },

                    .@"/=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(ass.value);
                        try c.emit(.div, &.{});
                    },

                    else => return c.newError("Invalid Assignment Operator", .{}),
                }

                const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
                try c.emit(op, &.{symbol.index}); // sapporra emite um pop
                try c.emit(.null, &.{}); // will be pop, no integer overflow. why?
                return;
            }

            if (ass.name.* == .index) {
                const left = ass.name.index.left;
                const index = ass.name.index.index;
                const value = ass.value;

                try c.compileExpression(left);
                try c.compileExpression(index);
                switch (ass.operator) {
                    .@"=" => {
                        try c.compileExpression(value);
                    },

                    .@"+=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.add, &.{});
                    },
                    .@"-=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.sub, &.{});
                    },

                    .@"*=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.mul, &.{});
                    },

                    .@"/=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.div, &.{});
                    },

                    else => return c.newError("Invalid Assignment Operator", .{}),
                }

                return try c.emit(.index_set, &.{});
            }

            if (ass.name.* == .method) {
                const left = ass.name.method.caller;
                const method: Value = .{ .tag = ass.name.method.method.value };
                const value = ass.value;

                try c.compileExpression(left);
                const pos = try c.addConstants(method);

                switch (ass.operator) {
                    .@"=" => {
                        try c.compileExpression(value);
                    },
                    .@"+=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.add, &.{});
                    },

                    .@"-=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.sub, &.{});
                    },

                    .@"*=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.mul, &.{});
                    },

                    .@"/=" => {
                        try c.compileExpression(ass.name);
                        try c.compileExpression(value);
                        try c.emit(.div, &.{});
                    },

                    else => return c.newError("Invalid Assignment Operator", .{}),
                }

                try c.emit(.method_set, &.{pos});
                return;
            }

            return c.newError("Invalid Assignment Operator", .{});
        },

        .infix => |infix| {
            const operator = infix.operator;

            if (operator == .@"<") {
                try c.compileExpression(infix.right);
                try c.compileExpression(infix.left);
                try c.emit(.gt, &.{});
                return;
            }

            if (operator == .@"<=") {
                try c.compileExpression(infix.right);
                try c.compileExpression(infix.left);
                try c.emit(.gte, &.{});
                return;
            }

            try c.compileExpression(infix.left);
            try c.compileExpression(infix.right);
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
                else => return c.newError("Unkow Operator", .{}),
            };
            try c.emit(op, &.{});
        },

        .prefix => |prefix| {
            const operator = prefix.operator;
            try c.compileExpression(prefix.right);
            switch (operator) {
                .@"!" => try c.emit(.not, &.{}),
                .@"-" => try c.emit(.min, &.{}),
                else => unreachable,
            }
        },

        .index => |index| {
            try c.compileExpression(index.left);
            try c.compileExpression(index.index);
            try c.emit(.index_get, &.{});
        },

        // TODO: rework
        .method => |method| {
            try c.compileExpression(method.caller);
            const pos = try c.addConstants(.{
                .tag = method.method.value,
            });
            try c.emit(.constant, &.{pos});
            try c.emit(.index_get, &.{});
            return;
        },

        .boolean => |boolean| {
            const op: code.Opcode = if (boolean.value) .true else .false;
            try c.emit(op, &.{});
        },

        .null => {
            try c.emit(.null, &.{});
        },

        // TODO: think !!
        .range => |range| {
            try c.compileExpression(range.end);
            try c.compileExpression(range.start);
            try c.emit(.set_range, &.{});
        },

        .integer => |int| {
            const pos = try c.addConstants(.{
                .integer = int.value,
            });
            try c.emit(.constant, &.{pos});
        },

        .tag => |tag| {
            const pos = try c.addConstants(.{
                .tag = tag.value,
                //.from = tag.from,
            });
            try c.emit(.constant, &.{pos});
        },

        .char => |char| {
            const pos = try c.addConstants(.{
                .char = char.value,
            });
            try c.emit(.constant, &.{pos});
        },

        .float => |float| {
            const pos = try c.addConstants(.{
                .float = float.value,
            });
            try c.emit(.constant, &.{pos});
        },

        .string => |str| {
            // const cvalue = try c.allocator.dupe(u8, str.value);
            // errdefer c.allocator.free(cvalue);
            var cvalue = try std.ArrayList(u8).initCapacity(c.allocator, str.value.len);
            errdefer cvalue.deinit();
            try cvalue.appendSlice(str.value);

            const obj = try c.allocator.create(Object);
            errdefer c.allocator.destroy(obj);
            obj.type = .{ .string = cvalue };

            const pos = try c.addConstants(.{ .obj = obj });
            try c.emit(.constant, &.{pos});
        },

        inline .array, .tuple => |array| {
            for (array.elements) |element| {
                try c.compileExpression(element);
            }
            try c.emit(.array, &.{array.elements.len});
        },

        .hash => |hash| {
            for (hash.pairs) |pair| {
                const key, const val = pair;
                try c.compileExpression(key);
                try c.compileExpression(val);
            }
            try c.emit(.hash, &.{hash.pairs.len * 2});
        },

        .call => |call| {
            try c.compileExpression(call.function);
            for (call.arguments) |arg| {
                try c.compileExpression(arg);
            }
            try c.emit(.call, &.{call.arguments.len});
        },

        .instance => |instance| {
            try c.compileExpression(instance.type);
            for (instance.fields) |field| {
                const ident = field.name;
                const pos = try c.addConstants(.{ .tag = ident.value });
                try c.emit(.constant, &.{pos});
                try c.compileExpression(field.value);
            }
            try c.emit(.instance, &.{instance.fields.len});
        },

        .function => |func| {
            try c.enterScope();

            // TODO: ast
            if (func.name) |name| {
                _ = try c.symbols.?.defineFunctionName(name.value);
            }

            for (func.parameters) |parameter| {
                _ = try c.symbols.?.define(parameter.value);
            }

            try c.compileStatement(.{ .block = func.body });

            // return the last value if no return statement
            if (c.lastInstructionIs(.pop)) try c.replaceLastPopWithReturn();

            // return null
            if (!c.lastInstructionIs(.retv)) try c.emit(.retn, &.{});

            const free_symbols = c.symbols.?.frees;
            const obj = try c.allocator.create(Object);
            errdefer c.allocator.destroy(obj);
            const num_locals = if (c.symbols) |s| s.def_number else 0;

            for (free_symbols.items) |symb| {
                try c.loadSymbol(symb);
            }
            const num_free_symbols = free_symbols.items.len;

            const instructions = try c.leaveScope();
            errdefer c.allocator.free(instructions);

            obj.type = .{ .closure = .{
                .func = .{
                    .instructions = instructions,
                    .num_locals = num_locals,
                    .num_parameters = func.parameters.len,
                },
            } };

            const pos = try c.addConstants(.{ .obj = obj });
            try c.emit(.closure, &.{ pos, num_free_symbols });
        },

        .@"if" => |ifexp| {
            try c.compileExpression(ifexp.condition);

            // since a new scope will be created, we need the counter
            // const len = c.insLen();
            const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

            if (ifexp.consequence.statements.len == 0) {
                try c.emit(.null, &.{});
            } else {
                // compiling the consequence
                try c.compileStatement(.{ .block = ifexp.consequence });
                // statements add a pop in the end, wee drop the last pop (if return)
                if (c.lastInstructionIs(.pop)) c.removeLastPop();
            }

            // always jump: null is returned
            const jump_pos = try c.emitPos(.jump, &.{9999});
            const after_consequence_position = c.insLen();

            // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
            try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

            if (ifexp.alternative) |alt| {
                if (alt.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compileStatement(.{ .block = alt });
                    // statements add a pop in the end, wee drop the last pop (if return)
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }
            } else {
                try c.emit(.null, &.{});
            }

            const after_alternative_pos = c.insLen();
            // Replaces the 9999 (.jump) to the correct operand (after_alternative_pos);
            try c.changeOperand(jump_pos, after_alternative_pos);
        },

        .match => |match| {
            var jumps_pos_list = std.ArrayList(usize).init(c.allocator);
            defer jumps_pos_list.deinit();

            // try c.enterScope();
            for (match.arms) |arm| for (arm.condition) |condition| {
                try c.compileExpression(match.value);
                try c.compileExpression(condition);
                try c.emit(.eq, &.{});

                const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

                if (arm.block.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compileStatement(.{ .block = arm.block });
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }

                const jum_pos = try c.emitPos(.jump, &.{9999});
                try jumps_pos_list.append(jum_pos);

                const after_consequence_position = c.insLen();
                try c.changeOperand(jump_if_not_true_pos, after_consequence_position);
            };

            if (match.else_block) |block| {
                if (block.statements.len == 0) {
                    try c.emit(.null, &.{});
                } else {
                    try c.compileStatement(.{ .block = block });
                    if (c.lastInstructionIs(.pop)) c.removeLastPop();
                }
            } else {
                try c.emit(.null, &.{});
            }

            const after_alternative_pos = c.insLen();

            for (jumps_pos_list.items) |jum_pos| {
                try c.changeOperand(jum_pos, after_alternative_pos);
            }
        },

        .@"for" => |forloop| {
            const jump_pos = c.insLen();
            c.loop.info[c.loop.idx].start = jump_pos;

            try c.compileExpression(forloop.condition);

            // fake instruction position
            const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

            // compiling the consequence
            if (forloop.consequence.statements.len == 0) {
                try c.emit(.null, &.{});
            } else {
                try c.compileStatement(.{ .block = forloop.consequence });
                // statements add a pop in the end, wee drop the last pop (if return)
                if (c.lastInstructionIs(.pop)) c.removeLastPop();
            }

            try c.emit(.jump, &.{jump_pos});

            // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
            // const after_consequence_position = c.insLen() + start_pos;
            const after_consequence_position = c.insLen();
            try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

            // // break statement
            if (c.loop.info[c.loop.idx].end) |pox| {
                try c.changeOperand(pox, after_consequence_position);
                c.loop.info[c.loop.idx].end = null;
            } else {
                // // always jump: null is returned
                try c.emit(.null, &.{});
            }
        },

        .for_range => |*forloop| {
            try if (forloop.index == null) c.emitLoop(forloop) else c.emitLoopWithIndex(forloop);
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

            const pos_name = try c.addConstants(.{
                .tag = ty.name orelse "struct",
            });
            try c.emit(.constant, &.{pos_name});

            for (fields) |field| {
                const ident = field.name;
                const value = field.value;

                const pos = try c.addConstants(.{ .tag = ident.value });
                try c.emit(.constant, &.{pos});

                try c.compileExpression(value);
            }

            for (decls) |dec| {
                // function name as a field
                const pos = try c.addConstants(.{ .tag = dec.name.value });
                try c.emit(.constant, &.{pos});
                _ = try c.emitdecl(&dec);
            }

            // NOTE: will work?
            // const pos = c.addConstants(.{ .@"struct" = .{
            //     .fields_len = fields.len,
            //     .decl_len = decls.len,
            //     .name = name
            //     .instructions = ...
            // } })

            try c.emit(.type, &.{ decls.len + 1, fields.len, @intFromEnum(e) });
        },

        .class => |class| {
            for (class.fields) |field| {
                const pos = try c.addConstants(.{ .tag = field.name });
                try c.emit(.constant, &.{pos});
                try c.compileExpression(field.value);
            }
            for (class.decls) |decls| {
                const pos = try c.addConstants(.{ .tag = decls.name.value });
                try c.emit(.constant, &.{pos});
                try c.compileStatement(.{ .@"fn" = decls });
            }
            try c.emit(.class, &.{2 * (class.fields.len + class.decls.len)});
        },

        else => |exx| {
            return c.newError("Invalid Expression '{}'", .{exx});
        },
    }
}

fn define(c: *Compiler, name: []const u8) !void {
    if (c.symbols.?.store.contains(name)) return c.newError("Redeclaration of variable '{s}'", .{name});
    const symbol = try c.symbols.?.define(name);
    const op: code.Opcode = if (symbol.scope == .global) .setgv else .setlv;
    try c.emit(op, &.{symbol.index});
}

fn emitLoopWithIndex(c: *Compiler, forloop: *ast.ForRange) anyerror!void {
    // cast the expression as a range and load it as a constant
    const name = forloop.ident;
    const iname = forloop.index.?;
    defer {
        _ = c.symbols.?.store.swapRemove(name);
        _ = c.symbols.?.store.swapRemove(iname);
    }
    // TODO: dont do this shit!!!!
    const pos = try c.addConstants(.null);
    const ipos = try c.addConstants(.null);
    try c.emit(.constant, &.{ipos});
    try c.emit(.constant, &.{pos});
    try c.compileExpression(forloop.iterable);
    try c.emit(.to_range, &.{pos});

    const jump_pos = c.insLen();
    c.loop.info[c.loop.idx].start = jump_pos;
    // in this block, the last instruction must be boolean
    {
        try c.emit(.get_range_idx, &.{pos});
    }

    const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

    // compiling the consequence
    if (forloop.body.statements.len != 0) {
        // item
        try c.define(name);
        try c.define(iname);

        try c.compileStatement(.{ .block = forloop.body });
        // statements add a pop in the end, wee drop the last pop (if return)
        if (c.lastInstructionIs(.pop)) c.removeLastPop();
    }

    try c.emit(.jump, &.{jump_pos});

    // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
    const after_consequence_position = c.insLen();

    // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
    try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

    // // break statement
    if (c.loop.info[c.loop.idx].end) |pox| {
        try c.changeOperand(pox, after_consequence_position);
        c.loop.info[c.loop.idx].end = null;
    } else {
        // // always jump: null is returned
        try c.emit(.null, &.{});
    }
}

fn emitLoop(c: *Compiler, forloop: *ast.ForRange) anyerror!void {
    // cast the expression as a range and load it as a constant
    const name = forloop.ident;
    defer _ = c.symbols.?.store.swapRemove(name);

    // TODO: dont do this shit!!!!
    const pos = try c.addConstants(.null);
    try c.emit(.constant, &.{pos});
    try c.compileExpression(forloop.iterable);
    try c.emit(.to_range, &.{pos});

    const jump_pos = c.insLen();
    c.loop.info[c.loop.idx].start = jump_pos;
    // in this block, the last instruction must be boolean
    {
        try c.emit(.get_range, &.{pos});
    }

    const jump_if_not_true_pos = try c.emitPos(.jumpifnottrue, &.{9999});

    // compiling the consequence
    if (forloop.body.statements.len != 0) {
        try c.define(name);

        try c.compileStatement(.{ .block = forloop.body });
        // statements add a pop in the end, wee drop the last pop (if return)
        if (c.lastInstructionIs(.pop)) c.removeLastPop();
    }

    try c.emit(.jump, &.{jump_pos});

    // this is the real jumpifnottrue position. this is how deep the compiled forloop.consequence is
    const after_consequence_position = c.insLen();

    // Replaces the 9999 (.jump_if_not_true_pos) to the correct operand (after_consequence_position);
    try c.changeOperand(jump_if_not_true_pos, after_consequence_position);

    // // break statement
    if (c.loop.info[c.loop.idx].end) |pox| {
        try c.changeOperand(pox, after_consequence_position);
        c.loop.info[c.loop.idx].end = null;
    } else {
        // // always jump: null is returned
        try c.emit(.null, &.{});
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
fn removeLastPop(c: *Compiler) void {
    c.allocator.free(c.currentInstruction().pop() orelse unreachable);
    c.scopes.items[c.scope_index].last_ins = c.scopes.items[c.scope_index].prev_ins;
}

fn addConstants(c: *Compiler, val: Value) !usize {
    try c.constants.append(val);
    return c.constants.items.len - 1;
}

fn addInstruction(c: *Compiler, ins: code.Instructions) !usize {
    const pos_new_ins = c.currentInstruction().items.len;
    try c.currentInstruction().append(ins);
    return pos_new_ins;
}

fn replaceInstruction(c: *Compiler, pos: usize, new_ins: []u8) !void {
    c.allocator.free(c.currentInstruction().orderedRemove(pos));
    try c.currentInstruction().insert(pos, new_ins);
}

fn replaceLastPopWithReturn(c: *Compiler) !void {
    const last_pos = c.scopes.items[c.scope_index].last_ins.pos;
    try c.replaceInstruction(last_pos, try code.makeBytecode(c.allocator, .retv, &.{}));
    c.scopes.items[c.scope_index].last_ins.opcode = .retv;
}

/// replaces the instruction
fn changeOperand(c: *Compiler, op_pos: usize, operand: usize) !void {
    const op: code.Opcode = @enumFromInt(c.currentInstruction().items[op_pos][0]);
    const new_ins = try code.makeBytecode(c.allocator, op, &.{operand});
    try c.replaceInstruction(op_pos, new_ins);
}

/// generate a instruction and add it to a pool
pub fn emit(c: *Compiler, op: code.Opcode, operants: []const usize) !void {
    const ins = try code.makeBytecode(c.allocator, op, operants);
    const pos = try c.addInstruction(ins);
    c.setLastInstruction(op, pos);
}

pub fn emitPos(c: *Compiler, op: code.Opcode, operants: []const usize) !usize {
    const ins = try code.makeBytecode(c.allocator, op, operants);
    const pos = try c.addInstruction(ins);
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

pub fn bytecode(c: *Compiler) !Bytecode {
    const ins = try std.mem.concat(c.allocator, u8, c.currentInstruction().items);
    errdefer c.allocator.free(ins);
    return .{ .positions = try c.positions.toOwnedSlice(), .constants = c.constants.items, .instructions = ins };
}

fn findImportName(c: *Compiler, path: []const u8) ![]const u8 {
    if (!std.mem.endsWith(u8, path, ".loli")) {
        return c.newError("Invalid Module Path: expected .loli extention, got your mama", .{});
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

    pub fn deinit(b: *Bytecode, c: *const Compiler) void {
        c.allocator.free(b.instructions);
    }
};

test {
    // _ = @import("compiler_test.zig");
    // _ = SymbolTable;
    // _ = Scope;
}
