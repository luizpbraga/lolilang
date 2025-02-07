// TODO: SymbolTable should not allocate internals SymbolTable. use index
const std = @import("std");
/// symbles (identifier table)
/// Information such as its location, its scope, whether it was previously declared or not
const SymbolTable = @This();
/// identifier name to scope map
allocator: std.mem.Allocator,
store: std.StringArrayHashMap(Symbol),
frees: std.ArrayList(*Symbol),
def_number: usize = 0,
outer: ?*SymbolTable = null,

/// TODO: use another abstraction to deal with public and const values
pub const Symbol = struct {
    public: bool = false,
    con: bool = false,
    scope: ScopeType,
    name: []const u8,
    index: usize = 0,
};

pub const ScopeType = enum {
    global,
    local,
    builtin,
    free,
    function,
};

pub fn init(alloc: std.mem.Allocator) SymbolTable {
    return .{
        .allocator = alloc,
        .store = .init(alloc),
        .frees = .init(alloc),
        .def_number = 0,
    };
}

pub fn create(allocator: std.mem.Allocator) !*SymbolTable {
    const st = try allocator.create(SymbolTable);
    errdefer allocator.destroy(st);
    st.* = .init(allocator);
    for (0.., @import("builtins.zig").builtin_functions) |i, b| {
        _ = try st.defineBuiltin(i, b.name);
    }
    return st;
}

pub fn initEnclosed(outer: *SymbolTable) !*SymbolTable {
    var st = try outer.allocator.create(SymbolTable);
    errdefer outer.allocator.destroy(st);
    st.* = .init(outer.allocator);
    st.outer = outer;
    return st;
}

pub fn deinit(t: *SymbolTable) void {
    t.store.deinit();
    t.frees.deinit();
}

pub fn deinitEnclosed(t: *SymbolTable) void {
    t.store.deinit();
    t.frees.deinit();
    t.allocator.destroy(t);
}

pub fn defineBuiltin(t: *SymbolTable, index: usize, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .builtin,
        .index = index,
    };
    try t.store.put(name, symbol);
    return symbol;
}

pub fn define(t: *SymbolTable, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = if (t.outer == null) .global else .local,
        .index = t.def_number,
    };
    try t.store.put(name, symbol);
    t.def_number += 1;
    return symbol;
}

pub fn defineFunctionName(t: *SymbolTable, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .function,
        .index = 0,
    };
    try t.store.put(name, symbol);
    return symbol;
}

pub fn defineFree(t: *SymbolTable, original_symbol: *Symbol) !?*Symbol {
    try t.frees.append(original_symbol);
    const symbol: Symbol = .{
        .name = original_symbol.name,
        .scope = .free,
        .index = t.frees.items.len - 1,
    };
    try t.store.put(original_symbol.name, symbol);
    return t.store.getPtr(original_symbol.name);
}

pub fn defineLocal(t: *SymbolTable, name: []const u8) !?*Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .local,
        .index = t.def_number,
    };
    try t.store.put(name, symbol);
    t.def_number += 1;
    return t.store.getPtr(name);
}

pub fn resolve(s: *SymbolTable, name: []const u8) !?*Symbol {
    var obj = s.store.getPtr(name);

    if (obj == null and s.outer != null) {
        obj = try s.outer.?.resolve(name);

        if (obj == null) {
            return obj;
        }

        if (obj.?.scope == .global or obj.?.scope == .builtin) {
            return obj;
        }

        const free = try s.defineFree(obj.?);
        return free;
    }
    return obj;
}

test define {
    const talloc = std.testing.allocator;

    var expected: std.StringHashMap(Symbol) = .init(talloc);
    defer expected.deinit();

    try expected.put("a", .{ .name = "a", .scope = .global, .index = 0 });
    try expected.put("b", .{ .name = "b", .scope = .global, .index = 1 });
    try expected.put("c", .{ .name = "c", .scope = .local, .index = 0 });
    try expected.put("d", .{ .name = "d", .scope = .local, .index = 1 });
    try expected.put("e", .{ .name = "e", .scope = .local, .index = 0 });
    try expected.put("f", .{ .name = "f", .scope = .local, .index = 1 });

    var global: SymbolTable = .init(talloc);
    defer global.deinit();

    const a = try global.define("a");
    const aexp = expected.get("a").?;
    if (!std.meta.eql(a, aexp)) {
        return error.UnexpendedSymbol;
    }

    const b = try global.define("b");
    const bexp = expected.get("b").?;
    if (!std.meta.eql(b, bexp)) {
        return error.UnexpendedSymbol;
    }

    var local1 = try global.initEnclosed();
    defer local1.deinitEnclosed();

    const c = try local1.define("c");
    const cexp = expected.get("c").?;
    if (!std.meta.eql(c, cexp)) {
        std.log.err("expected {} got {}", .{ c, cexp });
        return error.UnexpendedSymbol;
    }

    const d = try local1.define("d");
    const dexp = expected.get("d").?;
    if (!std.meta.eql(d, dexp)) {
        return error.UnexpendedSymbol;
    }

    var local2 = try global.initEnclosed();
    defer local2.deinitEnclosed();

    const e = try local2.define("e");
    const eexp = expected.get("e").?;
    if (!std.meta.eql(e, eexp)) {
        return error.UnexpendedSymbol;
    }

    const f = try local2.define("f");
    const fexp = expected.get("f").?;
    if (!std.meta.eql(f, fexp)) {
        return error.UnexpendedSymbol;
    }
}

test "Resolve Local" {
    const talloc = std.testing.allocator;

    const expected: [4]Symbol = .{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "c", .scope = .local, .index = 0 },
        .{ .name = "d", .scope = .local, .index = 1 },
    };

    var global: SymbolTable = .init(talloc);
    defer global.deinit();

    _ = try global.define("a");
    _ = try global.define("b");

    var local = try global.initEnclosed();
    defer local.deinitEnclosed();

    _ = try local.define("c");
    _ = try local.define("d");

    for (expected) |sym| {
        const result = local.resolve(sym.name) orelse {
            std.log.err("expected name {s} with no null", .{sym.name});
            return error.FoundNull;
        };
        if (!std.meta.eql(sym, result.*)) {
            return error.UnexpendedSymbol;
        }
    }
}

test "Resolve Nexted Local" {
    const talloc = std.testing.allocator;

    var global = SymbolTable.init(talloc);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    var local1 = try global.initEnclosed();
    defer local1.deinitEnclosed();
    _ = try local1.define("c");
    _ = try local1.define("d");

    var local2 = try local1.initEnclosed();
    defer local2.deinitEnclosed();
    _ = try local2.define("e");
    _ = try local2.define("f");

    const tests: []const struct {
        table: *SymbolTable,
        expected: []const Symbol,
    } = &.{
        .{
            .table = local1,
            .expected = &.{
                .{ .name = "a", .scope = .global, .index = 0 },
                .{ .name = "b", .scope = .global, .index = 1 },
                .{ .name = "c", .scope = .local, .index = 0 },
                .{ .name = "d", .scope = .local, .index = 1 },
            },
        },

        .{
            .table = local2,
            .expected = &.{
                .{ .name = "a", .scope = .global, .index = 0 },
                .{ .name = "b", .scope = .global, .index = 1 },
                .{ .name = "e", .scope = .local, .index = 0 },
                .{ .name = "f", .scope = .local, .index = 1 },
            },
        },
    };

    for (tests) |t| {
        for (t.expected) |sym| {
            const result = t.table.resolve(sym.name) orelse return error.FoundNull;
            if (!std.meta.eql(sym, result.*)) {
                return error.UnexpendedSymbol;
            }
        }
    }
}
