const std = @import("std");

const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .@"fn" },
    .{ "var", .@"var" },
    .{ "const", .@"const" },
    .{ "return", .@"return" },
    .{ "true", .true },
    .{ "false", .false },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "and", .@"and" },
    .{ "or", .@"or" },
});

const TokenType = enum {
    illegal,
    eof,

    // identifiers + literals
    identifier, // add, foobar, x, y, ...
    int, // 123678

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
    @"=>",
    @"<",
    @">=",
    @"<=",

    // Delimiters
    @",",
    @";",

    @"(",
    @")",
    @"{",
    @"}",

    // keywords
    @"fn",
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

const Token = struct {
    type: TokenType,
    literal: []const u8,

    fn lookupIdentfier(ident: []const u8) TokenType {
        return if (keywords.has(ident)) keywords.get(ident).? else .identifier;
    }
};

const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: u8 = 0,

    fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        l.readChar();
        return l;
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) self.readChar();
        return self.input[position..self.position];
    }

    fn readChar(self: *Lexer) void {
        self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peekChar(self: *const Lexer) u8 {
        return if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
    }

    fn skipWhiteSpace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\r') self.readChar();
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        var position = self.position;
        while (isLetter(self.ch)) self.readChar();
        return self.input[position..self.position];
    }

    fn newToken(token_type: TokenType) Token {
        return .{
            .type = token_type,
            .literal = @tagName(token_type),
        };
    }
    fn nextToken(self: *Lexer) Token {
        defer self.readChar();
        self.skipWhiteSpace();
        return switch (self.ch) {
            '=' => switch (self.peekChar()) {
                '=' => newToken(.@"=="),
                '>' => newToken(.@"=>"),
                else => newToken(.@"="),
            },
            '!' => switch (self.peekChar()) {
                '=' => newToken(.@"!="),
                else => newToken(.@"!"),
            },
            '>' => newToken(.@">"),
            '<' => newToken(.@"<"),
            '/' => newToken(.@"/"),
            '*' => newToken(.@"*"),
            '+' => newToken(.@"+"),
            '-' => newToken(.@"-"),
            ';' => newToken(.@";"),
            '(' => newToken(.@"("),
            ')' => newToken(.@")"),
            '{' => newToken(.@"{"),
            '}' => newToken(.@"}"),
            0 => newToken(.eof),
            // identifiers
            'a'...'z', 'A'...'Z', '_' => x: {
                const literal = self.readIdentifier();
                const token_type = Token.lookupIdentfier(literal);
                break :x Token{ .type = token_type, .literal = literal };
            },

            '0'...'9' => x: {
                const token_type = TokenType.int;
                const literal = self.readNumber();
                break :x Token{ .type = token_type, .literal = literal };
            },
            else => newToken(.illegal),
        };
    }
};

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ('_' == ch);
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

pub fn main() !void {
    const input =
        \\var five = 5;
        \\var ten = 10;
        \\const add = fn(x, y) {
        \\  return (x + y) / 2 * 100 > 1;
        \\};
        \\var result = if (x > 2) add(five, ten) < else !true;
        \\const p = 5 == 3;
    ;

    var l = Lexer.init(input);
    while (true) {
        var t = l.nextToken();
        std.debug.print("{} {s}\n", .{ t.type, t.literal });
        if (t.type == .eof) break;
    }
}
