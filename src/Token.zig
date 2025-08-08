const std = @import("std");
const Self = @This();

tag: Tag,
loc: Loc = .{},
const Loc = struct { start: usize = 0, end: usize = 0 };

pub fn literal(s: *const Self) []const u8 {
    return @tagName(s.tag);
}

pub fn lookupIdentfier(ident: []const u8) Tag {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}

pub const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "class", .class },
    .{ "var", .@"var" },
    .{ "pub", .@"pub" },
    .{ "con", .con },
    .{ "in", .in },
    .{ "fn", .@"fn" },
    .{ "match", .match },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "continue", .@"continue" },
    .{ "or", .@"or" },
    .{ "and", .@"and" },
    .{ "true", .true },
    .{ "false", .false },
    .{ "for", .@"for" },
    .{ "null", .null },
    .{ "defer", .@"defer" },
    .{ "break", .@"break" },
    .{ "return", .@"return" },
    .{ "enum", .@"enum" },
    .{ "struct", .@"struct" },
    .{ "import", .import },
});

pub const Tag = enum {
    illegal,
    eof,

    new_line,
    comment,

    // identifiers + literals
    identifier, // add, foobar, x, y, ...
    float,
    char,
    integer, // 123678
    string, // "fuck u"
    null,

    // operators
    @"%",
    @"=",
    @":=",
    @"!",
    @"==",
    @"!=",
    @"+",
    @"-",
    @"++",
    @"--",
    @"/",
    @"*",
    @">",
    @"<",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @">=",
    @"<=",
    @"=>",

    // Delimiters
    @",",
    @";",
    @":",
    @"^",

    @"(",
    @")",
    @"{",
    @"}",
    @"[",
    @"]",
    // method
    @".",
    // range
    @"..",
    @"..=",
    @"...",

    // keywords
    @"fn",
    @"enum",
    @"struct",
    class,
    import,
    @"for",
    match,
    @"return",
    @"var",
    @"pub",
    con,
    false,
    true,
    in,
    @"if",
    @"else",
    @"or",
    @"and",
    @"continue",
    @"break",
    @"defer",
};
