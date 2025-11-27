const std = @import("std");
const Allocator = std.mem.Allocator;

// TODO: SymbolTable should not allocate internals SymbolTable. use index
const SymbolTable = @This();
/// symbles (identifier table)
/// Information such as its location, its scope, whether it was previously declared or not
/// identifier name to scope map
store: std.StringArrayHashMapUnmanaged(Symbol) = .empty,
frees: std.ArrayList(*Symbol) = .{},
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

pub const init: SymbolTable = .{
    .store = .empty,
    .frees = .{},
    .def_number = 0,
};

pub fn create(gpa: Allocator) !*SymbolTable {
    const st = try gpa.create(SymbolTable);
    errdefer gpa.destroy(st);
    st.* = .init;
    for (0.., @import("builtins.zig").builtin_functions) |i, b| {
        _ = try st.defineBuiltin(gpa, i, b.name);
    }
    return st;
}

pub fn initEnclosed(outer: *SymbolTable, gpa: Allocator) !*SymbolTable {
    var st = try gpa.create(SymbolTable);
    errdefer gpa.destroy(st);
    st.* = .init;
    st.outer = outer;
    return st;
}

pub fn deinit(t: *SymbolTable, gpa: Allocator) void {
    t.store.deinit(gpa);
    t.frees.deinit(gpa);
}

pub fn deinitEnclosed(t: *SymbolTable, gpa: Allocator) void {
    t.store.deinit(gpa);
    t.frees.deinit(gpa);
    gpa.destroy(t);
}

pub fn defineBuiltin(t: *SymbolTable, gpa: Allocator, index: usize, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .builtin,
        .index = index,
    };
    try t.store.put(gpa, name, symbol);
    return symbol;
}

pub fn define(t: *SymbolTable, gpa: Allocator, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = if (t.outer == null) .global else .local,
        .index = t.def_number,
    };
    try t.store.put(gpa, name, symbol);
    t.def_number += 1;
    return symbol;
}

pub fn defineFunctionName(t: *SymbolTable, gpa: Allocator, name: []const u8) !Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .function,
        .index = 0,
    };
    try t.store.put(gpa, name, symbol);
    return symbol;
}

pub fn defineFree(t: *SymbolTable, gpa: Allocator, original_symbol: *Symbol) !?*Symbol {
    try t.frees.append(gpa, original_symbol);
    const symbol: Symbol = .{
        .name = original_symbol.name,
        .scope = .free,
        .index = t.frees.items.len - 1,
    };
    try t.store.put(gpa, original_symbol.name, symbol);
    return t.store.getPtr(original_symbol.name);
}

pub fn defineLocal(t: *SymbolTable, gpa: Allocator, name: []const u8) !?*Symbol {
    const symbol: Symbol = .{
        .name = name,
        .scope = .local,
        .index = t.def_number,
    };
    try t.store.put(gpa, name, symbol);
    t.def_number += 1;
    return t.store.getPtr(name);
}

pub fn resolve(s: *SymbolTable, gpa: Allocator, name: []const u8) !?*Symbol {
    var obj = s.store.getPtr(name);

    if (obj == null and s.outer != null) {
        obj = try s.outer.?.resolve(gpa, name);

        if (obj == null) {
            return obj;
        }

        if (obj.?.scope == .global or obj.?.scope == .builtin) {
            return obj;
        }

        const free = try s.defineFree(gpa, obj.?);
        return free;
    }
    return obj;
}

// test define {
//     const gpa = std.testing.allocator;
//
//     var expected: std.StringHashMapUnmanaged(Symbol) = .empty;
//     defer expected.deinit(gpa);
//
//     try expected.put(gpa, "a", .{ .name = "a", .scope = .global, .index = 0 });
//     try expected.put(gpa, "b", .{ .name = "b", .scope = .global, .index = 1 });
//     try expected.put(gpa, "c", .{ .name = "c", .scope = .local, .index = 0 });
//     try expected.put(gpa, "d", .{ .name = "d", .scope = .local, .index = 1 });
//     try expected.put(gpa, "e", .{ .name = "e", .scope = .local, .index = 0 });
//     try expected.put(gpa, "f", .{ .name = "f", .scope = .local, .index = 1 });
//
//     var global: SymbolTable = .init(gpa);
//     defer global.deinit();
//
//     const a = try global.define("a");
//     const aexp = expected.get("a").?;
//     if (!std.meta.eql(a, aexp)) {
//         return error.UnexpendedSymbol;
//     }
//
//     const b = try global.define("b");
//     const bexp = expected.get("b").?;
//     if (!std.meta.eql(b, bexp)) {
//         return error.UnexpendedSymbol;
//     }
//
//     var local1 = try global.initEnclosed();
//     defer local1.deinitEnclosed();
//
//     const c = try local1.define("c");
//     const cexp = expected.get("c").?;
//     if (!std.meta.eql(c, cexp)) {
//         std.log.err("expected {} got {}", .{ c, cexp });
//         return error.UnexpendedSymbol;
//     }
//
//     const d = try local1.define("d");
//     const dexp = expected.get("d").?;
//     if (!std.meta.eql(d, dexp)) {
//         return error.UnexpendedSymbol;
//     }
//
//     var local2 = try global.initEnclosed();
//     defer local2.deinitEnclosed();
//
//     const e = try local2.define("e");
//     const eexp = expected.get("e").?;
//     if (!std.meta.eql(e, eexp)) {
//         return error.UnexpendedSymbol;
//     }
//
//     const f = try local2.define("f");
//     const fexp = expected.get("f").?;
//     if (!std.meta.eql(f, fexp)) {
//         return error.UnexpendedSymbol;
//     }
// }
//
// test "Resolve Local" {
//     const talloc = std.testing.allocator;
//
//     const expected: [4]Symbol = .{
//         .{ .name = "a", .scope = .global, .index = 0 },
//         .{ .name = "b", .scope = .global, .index = 1 },
//         .{ .name = "c", .scope = .local, .index = 0 },
//         .{ .name = "d", .scope = .local, .index = 1 },
//     };
//
//     var global: SymbolTable = .init(talloc);
//     defer global.deinit();
//
//     _ = try global.define("a");
//     _ = try global.define("b");
//
//     var local = try global.initEnclosed();
//     defer local.deinitEnclosed();
//
//     _ = try local.define("c");
//     _ = try local.define("d");
//
//     for (expected) |sym| {
//         const result = local.resolve(gpa, sym.name) orelse {
//             std.log.err("expected name {s} with no null", .{sym.name});
//             return error.FoundNull;
//         };
//         if (!std.meta.eql(sym, result.*)) {
//             return error.UnexpendedSymbol;
//         }
//     }
// }
//
// test "Resolve Nexted Local" {
//     const talloc = std.testing.allocator;
//
//     var global = SymbolTable.init(talloc);
//     defer global.deinit();
//     _ = try global.define("a");
//     _ = try global.define("b");
//
//     var local1 = try global.initEnclosed();
//     defer local1.deinitEnclosed();
//     _ = try local1.define("c");
//     _ = try local1.define("d");
//
//     var local2 = try local1.initEnclosed();
//     defer local2.deinitEnclosed();
//     _ = try local2.define("e");
//     _ = try local2.define("f");
//
//     const tests: []const struct {
//         table: *SymbolTable,
//         expected: []const Symbol,
//     } = &.{
//         .{
//             .table = local1,
//             .expected = &.{
//                 .{ .name = "a", .scope = .global, .index = 0 },
//                 .{ .name = "b", .scope = .global, .index = 1 },
//                 .{ .name = "c", .scope = .local, .index = 0 },
//                 .{ .name = "d", .scope = .local, .index = 1 },
//             },
//         },
//
//         .{
//             .table = local2,
//             .expected = &.{
//                 .{ .name = "a", .scope = .global, .index = 0 },
//                 .{ .name = "b", .scope = .global, .index = 1 },
//                 .{ .name = "e", .scope = .local, .index = 0 },
//                 .{ .name = "f", .scope = .local, .index = 1 },
//             },
//         },
//     };
//
//     for (tests) |t| {
//         for (t.expected) |sym| {
//             const result = t.table.resolve(gpa, sym.name) orelse return error.FoundNull;
//             if (!std.meta.eql(sym, result.*)) {
//                 return error.UnexpendedSymbol;
//             }
//         }
//     }
// }
