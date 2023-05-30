const std = @import("std");
const Token = @import("Token.zig");
const TokenType = Token.TokenType;
const Self = @This();

input: []const u8,
position: usize = 0,
read_position: usize = 0,
ch: u8 = 0,

pub fn init(input: []const u8) Self {
    var lexer = Self{ .input = input };
    lexer.readChar();
    return lexer;
}

fn readNumber(self: *Self) []const u8 {
    const position = self.position;
    while (isDigit(self.ch)) self.readChar();
    return self.input[position..self.position];
}

fn readChar(self: *Self) void {
    self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
    self.position = self.read_position;
    self.read_position += 1;
}

fn peekChar(self: *const Self) u8 {
    return if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
}

fn skipWhiteSpace(self: *Self) void {
    while (self.ch == ' ' or self.ch == '\n' or self.ch == '\t' or self.ch == '\r') self.readChar();
}

fn readIdentifier(self: *Self) []const u8 {
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

pub fn nextToken(self: *Self) Token {
    self.skipWhiteSpace();
    var tok = switch (self.ch) {
        // TODO: fix == and !=
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
        'a'...'z', 'A'...'Z', '_' => {
            const literal = self.readIdentifier();
            const token_type = Token.lookupIdentfier(literal);
            return Token{ .type = token_type, .literal = literal };
        },

        '0'...'9' => {
            const token_type = TokenType.int;
            const literal = self.readNumber();
            return Token{ .type = token_type, .literal = literal };
        },
        else => newToken(.illegal),
    };

    // this code is unrechable for identifiers
    self.readChar();
    return tok;
}

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ('_' == ch);
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}
