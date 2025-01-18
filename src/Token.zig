const Self = @This();

type: Type,
literal: []const u8,
at: usize = 0,
comment_pos: ?[2]usize = null,

const std = @import("std");

pub fn lookupIdentfier(ident: []const u8) Type {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}

pub const keywords = std.StaticStringMap(Type).initComptime(.{
    .{ "var", .@"var" },
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
    .{ "with", .with },
    .{ "struct", .@"struct" },
    .{ "new", .new },
});

pub const Type = enum {
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
    new,
    @"for",
    match,
    @"return",
    @"var",
    con,
    false,
    true,
    in,
    with,
    @"if",
    @"else",
    @"or",
    @"and",
    @"continue",
    @"break",
    @"defer",
};
