const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("asc.zig");
const Self = @This();
lexer: *Lexer,
cur_token: Token,
peek_token: Token,

const ParseError = error{
    UnexpectToken,
};

fn nextToken(self: *Self) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

pub fn new(lexer: *Lexer) Self {
    var p = Self{
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
    };

    p.nextToken();
    p.nextToken();

    return p;
}

fn curTokenIs(self: *Self, token_type: Token.TokenType) bool {
    return self.cur_token.type == token_type;
}

fn peekTokenIs(self: *Self, token_type: Token.TokenType) bool {
    return self.peek_token.type == token_type;
}

fn expectPeek(self: *Self, token_type: Token.TokenType) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    } else {
        return false;
    }
}

pub fn parseReturnStatement(self: *Self) ParseError!ast.ReturnStatement {
    var stmt = ast.ReturnStatement{
        .token = self.cur_token,
        .value = undefined,
    };

    self.nextToken();

    while (!self.curTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

pub fn parseVarStatement(self: *Self) ParseError!ast.VarStatement {
    var stmt = ast.VarStatement{
        .token = self.cur_token,
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
        return error.UnexpectToken;
    }

    stmt.name = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        std.log.err("Expect Assignment Token (=), found '{s}'", .{self.cur_token.literal});
        return error.UnexpectToken;
    }

    // TODO: evaluate the expression before the ';'
    while (!self.curTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

fn parseStatement(self: *Self) !?ast.Statement {
    return switch (self.cur_token.type) {
        .@"var" => .{ .var_statement = try self.parseVarStatement() },
        .@"return" => .{ .return_statement = try self.parseReturnStatement() },
        else => null,
    };
}

pub fn parseProgram(self: *Self, allocator: std.mem.Allocator) !ast.Program {
    var program = ast.Program{
        .statements = std.ArrayList(ast.Statement).init(allocator),
    };

    while (self.cur_token.type != .eof) {
        var stmt = try self.parseStatement();
        if (stmt) |stmt_val|
            try program.statements.append(stmt_val);
        self.nextToken();
    }

    return program;
}
