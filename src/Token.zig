const Self = @This();

type: TokenType,
literal: []const u8,

const std = @import("std");

pub fn lookupIdentfier(ident: []const u8) TokenType {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "in", .in },
    .{ "or", .@"or" },
    .{ "if", .@"if" },
    .{ "null", .null },
    .{ "func", .func },
    .{ "with", .with },
    .{ "true", .true },
    .{ "for", .@"for" },
    .{ "var", .@"var" },
    .{ "and", .@"and" },
    .{ "false", .false },
    .{ "else", .@"else" },
    .{ "enum", .@"enum" },
    .{ "defer", .@"defer" },
    .{ "const", .@"const" },
    .{ "break", .@"break" },
    .{ "return", .@"return" },
    .{ "switch", .@"switch" },
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
    func,
    @"enum",
    @"for",
    @"switch",
    @"return",
    @"var",
    @"const",
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
