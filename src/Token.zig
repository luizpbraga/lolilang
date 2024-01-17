const Self = @This();

type: TokenType,
literal: []const u8,

const std = @import("std");

pub fn lookupIdentfier(ident: []const u8) TokenType {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "int", .int },
    .{ "float", .float },
    .{ "string", .string },
    .{ "char", .char },
    .{ "null", .null },
    .{ "in", .in },
    .{ "or", .@"or" },
    .{ "if", .@"if" },
    .{ "null", .null },
    .{ "fn", .@"fn" },
    .{ "with", .with },
    .{ "true", .true },
    .{ "for", .@"for" },
    .{ "struct", .@"struct" },
    .{ "var", .@"var" },
    .{ "con", .con },
    .{ "and", .@"and" },
    .{ "false", .false },
    .{ "else", .@"else" },
    .{ "enum", .@"enum" },
    .{ "defer", .@"defer" },
    .{ "break", .@"break" },
    .{ "return", .@"return" },
    .{ "match", .match },
    .{ "continue", .@"continue" },
});

pub const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    identifier, // add, foobar, x, y, ...
    float,
    int, // 123678
    string, // "fuck u"
    char,
    null,

    // operators
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
    @"...",

    // keywords
    @"fn",
    @"enum",
    @"for",
    match,
    @"struct",
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
