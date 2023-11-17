const Self = @This();

type: TokenType,
literal: []const u8,

const std = @import("std");

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "in", .in },
    .{ "defer", .@"defer" },
    .{ "continue", .@"continue" },
    .{ "break", .@"break" },
    .{ "func", .func },
    .{ "enum", .@"enum" },
    .{ "for", .@"for" },
    .{ "switch", .@"switch" },
    .{ "if", .@"if" },
    .{ "true", .true },
    .{ "false", .false },
    .{ "else", .@"else" },
    .{ "and", .@"and" },
    .{ "or", .@"or" },
    .{ "var", .@"var" },
    .{ "const", .@"const" },
    .{ "return", .@"return" },
    .{ "null", .null },
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
    @"if",
    @"else",
    @"or",
    @"and",
    @"continue",
    @"break",
    @"defer",
};

pub fn lookupIdentfier(ident: []const u8) TokenType {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}
