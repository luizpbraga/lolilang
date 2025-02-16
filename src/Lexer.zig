const Self = @This();
input: []const u8,
position: usize = 0,
read_position: usize = 0,
line_index: usize = 0,
ch: u8 = 0,

const std = @import("std");
const Token = @import("Token.zig");
const Tag = Token.Tag;

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

    if (self.ch != '.') return .{ .tag = .integer, .loc = .{ .start = position, .end = self.position } };

    // todo: use token
    if (self.peekChar() == '.') return .{ .tag = .integer, .loc = .{ .start = position, .end = self.position } };

    self.readChar();

    while (isDigit(self.ch)) self.readChar();

    return .{ .tag = .float, .loc = .{ .start = position, .end = self.position } };
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

fn readIdentifierEndPos(self: *Self) !usize {
    if (isDigit(self.ch) and self.ch != '_') return error.InvelidDigitCharacterOnItendifierName;
    while (isLetter(self.ch) or isDigit(self.ch)) self.readChar();
    return self.position;
}

fn readString(self: *Self) []const u8 {
    const position = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch == '"' or self.ch == 0) break;
    }
    return self.input[position..self.position];
}

fn readEndPos(self: *Self, char: u8) usize {
    while (true) {
        self.readChar();
        if (self.ch == char or self.ch == 0) break;
    }
    return self.position;
}

fn readCharacter(self: *Self) []const u8 {
    const position = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch == '\'' or self.ch == 0) break;
    }
    return self.input[position..self.position];
}

fn newToken(self: *Self, tag: Tag) Token {
    return .{ .tag = tag, .loc = .{ .start = self.position, .end = self.position + 1 } };
}

pub fn nextToken(self: *Self) Token {
    self.skipWhiteSpace();

    if (self.ch == '#') {
        self.skipComments();
        return self.nextToken();
    }

    const tok: Token = switch (self.ch) {
        // '#' => {
        //     const start = self.position;
        //     self.skipComments();
        //     const end = self.position;
        //
        //     return .{ .}
        // },
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
        '\'' => x: {
            const start = self.position + 1;
            const end = self.readEndPos('\'');
            break :x .{ .tag = .char, .loc = .{ .start = start, .end = end } };
        },
        '"' => x: {
            const start = self.position + 1;
            const end = self.readEndPos('"');
            break :x .{ .tag = .string, .loc = .{ .start = start, .end = end } };
        },
        0 => self.newToken(.eof),
        // identifiers
        'a'...'z', 'A'...'Z', '_', '@' => {
            // TODO: handle error
            const start = self.position;
            const end = self.readIdentifierEndPos() catch unreachable;
            const tag = Token.lookupIdentfier(self.input[start..end]);
            return .{ .tag = tag, .loc = .{ .start = start, .end = end } };
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
