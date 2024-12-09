const Self = @This();
input: []const u8,
position: usize = 0,
read_position: usize = 0,
starting_line_position: usize = 0,
ch: u8 = 0,

const std = @import("std");
const Token = @import("Token.zig");
const TokenType = Token.Type;

pub fn init(input: []const u8) Self {
    var lexer = Self{ .input = input };
    lexer.readChar();
    return lexer;
}

// TODO: support Hexadecimal
fn readNumber(self: *Self) Token {
    const position = self.position;

    while (isDigit(self.ch)) {
        self.readChar();
    }

    if (self.ch != '.') return .{ .literal = self.input[position..self.position], .type = .int };

    if (self.peekChar() == '.') return .{ .literal = self.input[position..self.position], .type = .int };

    self.readChar();

    while (isDigit(self.ch)) self.readChar();

    return .{ .literal = self.input[position..self.position], .type = .float };
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
    while (self.ch == ' ' or self.ch == '\n' or self.ch == '\t' or self.ch == '\r') {
        if (self.ch == '\n') self.starting_line_position = self.read_position;
        self.readChar();
    }
}

fn skipComments(self: *Self) void {
    while (self.ch != '\n' and self.ch != 0) {
        self.readChar();
    }

    self.skipWhiteSpace();
}

fn readIdentifier(self: *Self) ![]const u8 {
    const position = self.position;

    if (isDigit(self.ch)) return error.InvelidDigitCharacterOnItendifierName;

    while (isLetter(self.ch) or isDigit(self.ch)) self.readChar();
    return self.input[position..self.position];
}

fn readString(self: *Self) []const u8 {
    const position = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch == '"' or self.ch == 0) break;
    }
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

    // if (self.ch == '/' and self.peekChar() == '/') {
    if (self.ch == '#') {
        self.skipComments();
        return self.nextToken();
    }

    const tok = switch (self.ch) {
        '^' => newToken(.@"^"),
        ';' => newToken(.@";"),
        ',' => newToken(.@","),
        '(' => newToken(.@"("),
        ')' => newToken(.@")"),
        '{' => newToken(.@"{"),
        '}' => newToken(.@"}"),
        '[' => newToken(.@"["),
        ']' => newToken(.@"]"),
        '=' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"==");
            },
            '>' => x: {
                self.readChar();
                break :x newToken(.@"=>");
            },
            else => newToken(.@"="),
        },
        '!' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"!=");
            },
            else => newToken(.@"!"),
        },
        '>' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@">=");
            },
            else => newToken(.@">"),
        },
        '<' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"<=");
            },
            else => newToken(.@"<"),
        },
        '+' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"+=");
            },
            '+' => x: {
                self.readChar();
                break :x newToken(.@"++");
            },
            else => newToken(.@"+"),
        },
        '-' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"-=");
            },
            '-' => x: {
                self.readChar();
                break :x newToken(.@"--");
            },
            else => newToken(.@"-"),
        },
        '*' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"*=");
            },
            else => newToken(.@"*"),
        },
        '.' => switch (self.peekChar()) {
            '.' => x: {
                self.readChar();
                break :x newToken(.@"..");
            },
            else => newToken(.@"."),
        },
        '/' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@"/=");
            },
            else => newToken(.@"/"),
        },
        ':' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x newToken(.@":=");
            },
            else => newToken(.@":"),
        },
        '"' => Token{
            .type = .string,
            .literal = self.readString(),
        },
        0 => newToken(.eof),
        // identifiers
        'a'...'z', 'A'...'Z', '_' => {
            // TODO: handle error
            const literal = self.readIdentifier() catch unreachable;
            const token_type = Token.lookupIdentfier(literal);
            return Token{ .type = token_type, .literal = literal };
        },
        '0'...'9' => {
            return self.readNumber();
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
    return ('0' <= ch and ch <= '9') or ch == '_';
}
