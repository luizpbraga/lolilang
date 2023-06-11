const Self = @This();
type: TokenType,
literal: []const u8,

const std = @import("std");

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .@"fn" },
    .{ "enum", .@"enum" },
    .{ "for", .@"for" },
    .{ "switch", .@"switch" },
    .{ "if", .@"if" },
    .{ "or", .@"or" },
    .{ "true", .true },
    .{ "and", .@"and" },
    .{ "var", .@"var" },
    .{ "false", .false },
    .{ "else", .@"else" },
    .{ "const", .@"const" },
    .{ "return", .@"return" },
});

pub const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    identifier, // add, foobar, x, y, ...
    int, // 123678
    string, // "fuck u"

    // operators
    @"=",
    @"!",
    @"==",
    @"!=",
    @"+",
    @"-",
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
    @"fn",
    @"enum",
    @"for",
    @"switch",
    @"return",
    @"var",
    @"const",
    false,
    true,
    @"if",
    @"else",
    @"or",
    @"and",
};

pub fn lookupIdentfier(ident: []const u8) TokenType {
    return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
}
