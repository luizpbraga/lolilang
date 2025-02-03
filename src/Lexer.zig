const Self = @This();
input: []const u8,
position: usize = 0,
read_position: usize = 0,
line_index: usize = 0,
current_comment: ?[2]usize = null,
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

    // FIX: disallow multiples "_" in sequence on numbers literals
    while (isDigit(self.ch)) {
        self.readChar();
    }

    if (self.ch != '.') return self.newTokenLiteral(.integer, self.input[position..self.position]);

    if (self.peekChar() == '.') return self.newTokenLiteral(.integer, self.input[position..self.position]);

    self.readChar();

    while (isDigit(self.ch)) self.readChar();

    return self.newTokenLiteral(.float, self.input[position..self.position]);
}

fn readChar(self: *Self) void {
    self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
    self.position = self.read_position;
    self.read_position += 1;

    if (self.ch == '\n') {
        self.line_index += 1;
    }
}

fn peekChar(self: *const Self) u8 {
    return if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
}

fn skipWhiteSpace(self: *Self) void {
    while (self.ch == ' ' or self.ch == '\n' or self.ch == '\t' or self.ch == '\r') {
        self.readChar();
    }
}

fn skipComments(self: *Self) void {
    while (self.ch != '\n' and self.ch != 0) {
        self.readChar();
    }

    self.skipWhiteSpace();

    // a block of comments is a single comment
    if (self.ch == '#') self.skipComments();
}

fn readIdentifier(self: *Self) ![]const u8 {
    const position = self.position;

    if (isDigit(self.ch) and self.ch != '_') return error.InvelidDigitCharacterOnItendifierName;

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

fn readCharacter(self: *Self) []const u8 {
    const position = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch == '\'' or self.ch == 0) break;
    }
    return self.input[position..self.position];
}

fn newToken(self: *Self, token_type: TokenType) Token {
    const literal = @tagName(token_type);
    return self.newTokenLiteral(token_type, literal);
}

fn newTokenLiteral(self: *Self, token_type: TokenType, literal: []const u8) Token {
    defer self.current_comment = null;
    return .{ .type = token_type, .literal = literal, .at = self.position, .comment_pos = self.current_comment };
}

pub fn nextToken(self: *Self) Token {
    self.skipWhiteSpace();

    // if (self.ch == '#') {
    //     self.skipComments();
    //     return self.nextToken();
    // }

    const tok = switch (self.ch) {
        '#' => {
            const start = self.position;
            self.skipComments();
            const end = self.position;
            self.current_comment = .{ start, end };
            return self.nextToken();
        },
        '^' => self.newToken(.@"^"),
        '%' => self.newToken(.@"%"),
        ';' => self.newToken(.@";"),
        ',' => self.newToken(.@","),
        '(' => self.newToken(.@"("),
        ')' => self.newToken(.@")"),
        '{' => self.newToken(.@"{"),
        '}' => self.newToken(.@"}"),
        '[' => self.newToken(.@"["),
        ']' => self.newToken(.@"]"),
        '=' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"==");
            },
            '>' => x: {
                self.readChar();
                break :x self.newToken(.@"=>");
            },
            else => self.newToken(.@"="),
        },
        '!' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"!=");
            },
            else => self.newToken(.@"!"),
        },
        '>' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@">=");
            },
            else => self.newToken(.@">"),
        },
        '<' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"<=");
            },
            else => self.newToken(.@"<"),
        },
        '+' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"+=");
            },
            '+' => x: {
                self.readChar();
                break :x self.newToken(.@"++");
            },
            else => self.newToken(.@"+"),
        },
        '-' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"-=");
            },
            '-' => x: {
                self.readChar();
                break :x self.newToken(.@"--");
            },
            else => self.newToken(.@"-"),
        },
        '*' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"*=");
            },
            else => self.newToken(.@"*"),
        },
        '.' => switch (self.peekChar()) {
            '.' => x: {
                self.readChar();

                if (self.peekChar() == '=') {
                    self.readChar();
                    break :x self.newToken(.@"..=");
                }

                break :x self.newToken(.@"..");
            },
            else => self.newToken(.@"."),
        },
        '/' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@"/=");
            },
            else => self.newToken(.@"/"),
        },
        ':' => switch (self.peekChar()) {
            '=' => x: {
                self.readChar();
                break :x self.newToken(.@":=");
            },
            else => self.newToken(.@":"),
        },
        '\'' => self.newTokenLiteral(.char, self.readCharacter()),
        '"' => self.newTokenLiteral(.string, self.readString()),
        0 => self.newToken(.eof),
        // identifiers
        'a'...'z', 'A'...'Z', '_', '@' => {
            // TODO: handle error
            const literal = self.readIdentifier() catch |err| {
                @panic(@errorName(err));
            };
            const token_type = Token.lookupIdentfier(literal);
            return self.newTokenLiteral(token_type, literal);
        },
        '0'...'9' => {
            return self.readNumber();
        },
        else => self.newToken(.illegal),
    };

    // this code is unrechable for identifiers
    self.readChar();
    return tok;
}

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ('_' == ch) or (ch == '@');
}

fn isDigit(ch: u8) bool {
    return ('0' <= ch and ch <= '9') or ch == '_';
}
