const Token = @import("Token.zig");

const Lexer = @This();

input: []const u8,
position: usize = 0,
read_position: usize = 0,
line_index: usize = 0,
ch: u8 = 0,

pub fn init(input: []const u8) Lexer {
    var lexer = Lexer{ .input = input };
    lexer.readChar();
    return lexer;
}

// TODO: support Hexadecimal
fn readNumber(l: *Lexer) Token {
    const position = l.position;
    // FIX: disallow multiples "_" in sequence on numbers literals
    while (isDigit(l.ch)) {
        l.readChar();
    }
    if (l.ch != '.') return .{ .tag = .integer, .loc = .{ .start = position, .end = l.position } };
    // todo: use token
    if (l.peekChar() == '.') return .{ .tag = .integer, .loc = .{ .start = position, .end = l.position } };
    l.readChar();
    while (isDigit(l.ch)) l.readChar();
    return .{ .tag = .float, .loc = .{ .start = position, .end = l.position } };
}

fn readChar(l: *Lexer) void {
    l.ch = if (l.read_position >= l.input.len) 0 else l.input[l.read_position];
    l.position = l.read_position;
    l.read_position += 1;
    if (l.ch == '\n') {
        l.line_index += 1;
    }
}

fn peekChar(l: *const Lexer) u8 {
    return if (l.read_position >= l.input.len) 0 else l.input[l.read_position];
}

fn skipWhiteSpace(l: *Lexer) void {
    while (l.ch == ' ' or l.ch == '\n' or l.ch == '\t' or l.ch == '\r') l.readChar();
}

fn skipComments(l: *Lexer) void {
    while (l.ch != '\n' and l.ch != 0) l.readChar();
    l.skipWhiteSpace();
    if (l.ch == '#') l.skipComments();
}

fn readIdentifier(l: *Lexer) ![]const u8 {
    const position = l.position;
    if (isDigit(l.ch) and l.ch != '_') return error.InvelidDigitCharacterOnItendifierName;
    while (isLetter(l.ch) or isDigit(l.ch)) l.readChar();
    return l.input[position..l.position];
}

fn readIdentifierEndPos(l: *Lexer) !usize {
    if (isDigit(l.ch) and l.ch != '_') return error.InvelidDigitCharacterOnItendifierName;
    while (isLetter(l.ch) or isDigit(l.ch)) l.readChar();
    return l.position;
}

fn readString(l: *Lexer) []const u8 {
    const position = l.position + 1;
    while (true) {
        l.readChar();
        if (l.ch == '"' or l.ch == 0) break;
    }
    return l.input[position..l.position];
}

fn readEndPos(l: *Lexer, char: u8) usize {
    while (true) {
        l.readChar();
        if (l.ch == char or l.ch == 0) break;
    }
    return l.position;
}

fn readCharacter(l: *Lexer) []const u8 {
    const position = l.position + 1;
    while (true) {
        l.readChar();
        if (l.ch == '\'' or l.ch == 0) break;
    }
    return l.input[position..l.position];
}

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ('_' == ch) or (ch == '@');
}

fn isDigit(ch: u8) bool {
    // BUG
    return ('0' <= ch and ch <= '9') or ch == '_';
}

fn tokenFrom(l: *Lexer, tag: Token.Tag) Token {
    return .{ .tag = tag, .loc = .{ .start = l.position, .end = l.position + 1 } };
}

pub fn nextToken(l: *Lexer) Token {
    l.skipWhiteSpace(); // count it?
    if (l.ch == '#') {
        l.skipComments();
        return l.nextToken();
    }
    const tok: Token = switch (l.ch) {
        '^' => l.tokenFrom(.@"^"),
        '%' => l.tokenFrom(.@"%"),
        ';' => l.tokenFrom(.@";"),
        ',' => l.tokenFrom(.@","),
        '(' => l.tokenFrom(.@"("),
        ')' => l.tokenFrom(.@")"),
        '{' => l.tokenFrom(.@"{"),
        '}' => l.tokenFrom(.@"}"),
        '[' => l.tokenFrom(.@"["),
        ']' => l.tokenFrom(.@"]"),
        '=' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"==");
            },
            '>' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"=>");
            },
            else => l.tokenFrom(.@"="),
        },
        '!' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"!=");
            },
            else => l.tokenFrom(.@"!"),
        },
        '>' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@">=");
            },
            else => l.tokenFrom(.@">"),
        },
        '<' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"<=");
            },
            else => l.tokenFrom(.@"<"),
        },
        '+' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"+=");
            },
            '+' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"++");
            },
            else => l.tokenFrom(.@"+"),
        },
        '-' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"-=");
            },
            '-' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"--");
            },
            else => l.tokenFrom(.@"-"),
        },
        '*' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"*=");
            },
            else => l.tokenFrom(.@"*"),
        },
        '.' => switch (l.peekChar()) {
            '.' => x: {
                l.readChar();
                if (l.peekChar() == '=') {
                    l.readChar();
                    break :x l.tokenFrom(.@"..=");
                }
                break :x l.tokenFrom(.@"..");
            },
            else => l.tokenFrom(.@"."),
        },
        '/' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@"/=");
            },
            else => l.tokenFrom(.@"/"),
        },
        ':' => switch (l.peekChar()) {
            '=' => x: {
                l.readChar();
                break :x l.tokenFrom(.@":=");
            },
            else => l.tokenFrom(.@":"),
        },
        '\'' => x: {
            const start = l.position + 1;
            const end = l.readEndPos('\'');
            break :x .{ .tag = .char, .loc = .{ .start = start, .end = end } };
        },
        '"' => x: {
            const start = l.position + 1;
            const end = l.readEndPos('"');
            break :x .{ .tag = .string, .loc = .{ .start = start, .end = end } };
        },
        0 => l.tokenFrom(.eof),
        // identifiers
        'a'...'z', 'A'...'Z', '_', '@' => {
            // TODO: handle error
            const start = l.position;
            const end = l.readIdentifierEndPos() catch unreachable;
            const tag = Token.lookUpIdentifier(l.input[start..end]);
            return .{ .tag = tag, .loc = .{ .start = start, .end = end } };
        },
        '0'...'9' => return l.readNumber(),
        else => l.tokenFrom(.illegal),
    };
    // this code is unrechable for identifiers
    l.readChar();
    return tok;
}
