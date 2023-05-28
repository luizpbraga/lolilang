const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("asc.zig");

const Self = @This();
const prefixParseFn = *const fn (*Self) anyerror!ast.Expression;
const infixParseFn = *const fn (ast.Expression) anyerror!ast.Expression;

lexer: *Lexer,
cur_token: Token,
peek_token: Token,
allocator: std.mem.Allocator,
trash: std.ArrayList(*ast.Expression),
// TODO: iniciar esses caras
infix_parse_fns: std.AutoHashMap(Token.TokenType, infixParseFn),
prefix_parse_fns: std.AutoHashMap(Token.TokenType, prefixParseFn),

fn registerPrefix(self: *Self, token_type: Token.TokenType, func: prefixParseFn) void {
    self.prefix_parse_fns.put(token_type, func) catch unreachable;
}

fn registerInfix(self: *Self, token_type: Token.TokenType, func: infixParseFn) void {
    self.infix_parse_fns.put(token_type, func) catch unreachable;
}

const ParseError = error{
    UnexpectToken,
};

fn nextToken(self: *Self) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

pub fn new(allocator: std.mem.Allocator, lexer: *Lexer) Self {
    var prefix_parse_fns = std.AutoHashMap(Token.TokenType, prefixParseFn).init(allocator);

    var p = Self{
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .prefix_parse_fns = prefix_parse_fns,
        .infix_parse_fns = undefined,
        .allocator = allocator,
        .trash = undefined,
    };
    p.trash = std.ArrayList(*ast.Expression).init(p.allocator);

    p.nextToken();
    p.nextToken();

    p.registerPrefix(.identifier, parseIdentifier);
    p.registerPrefix(.int, parseIntegerLiteral);
    p.registerPrefix(.@"!", parsePrefixExpression);
    p.registerPrefix(.@"-", parsePrefixExpression);

    // std.testing.expect(p.prefix_parse_fns.count() == 4) catch unreachable;

    return p;
}
/// TODO: free infix_parse_fns when it was implemented
pub fn deinit(self: *Self) void {
    self.prefix_parse_fns.deinit();
    // for (self.trash.items) |f| self.allocator.destroy(f);
    self.trash.deinit();
    // self.infix_parse_fns.deinit();
}

fn parseIdentifier(self: *Self) anyerror!ast.Expression {
    return .{ .identifier = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    } };
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

fn parseReturnStatement(self: *Self) ParseError!ast.ReturnStatement {
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

fn parseVarStatement(self: *Self) ParseError!ast.VarStatement {
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

fn parseStatement(self: *Self) !ast.Statement {
    return switch (self.cur_token.type) {
        .@"var" => .{ .var_statement = try self.parseVarStatement() },
        .@"return" => .{ .return_statement = try self.parseReturnStatement() },
        else => .{ .expression_statement = try self.parseExpressionStatement() },
    };
}

pub const Precedence = enum {
    lower,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,
};

fn parseExpressionStatement(self: *Self) anyerror!ast.ExpressionStatement {
    const exp = try self.parseExpression(Precedence.lower);
    var stmt = ast.ExpressionStatement{
        .token = self.cur_token,
        .expression = exp,
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

fn parseExpression(self: *Self, precedence: Precedence) !?ast.Expression {
    _ = precedence;
    const prefix_fn = self.prefix_parse_fns.get(self.cur_token.type) orelse return null;
    const letfExp = try prefix_fn(self);
    return letfExp;
}

pub fn parseProgram(self: *Self, allocator: std.mem.Allocator) !ast.Program {
    var program = ast.Program{
        .statements = std.ArrayList(ast.Statement).init(allocator),
    };
    var stmt: ast.Statement = undefined;
    while (self.cur_token.type != .eof) {
        stmt = try self.parseStatement();
        try program.statements.append(stmt);
        self.nextToken();
    }
    return program;
}

fn parseIntegerLiteral(self: *Self) anyerror!ast.Expression {
    return .{ .integer_literal = ast.IntegerLiteral{
        .token = self.cur_token,
        .value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch |err| {
            std.log.err("Could not parse {s} as integer", .{self.cur_token.literal});
            return err;
        },
    } };
}

fn parsePrefixExpression(self: *Self) anyerror!ast.Expression {
    var pre_expression = ast.PrefixExpression{
        .operator = self.cur_token.literal,
        .token = self.cur_token,
    };

    self.nextToken();
    var exp = try self.parseExpression(Precedence.prefix);
    if (exp) |*e| {
        var cpy = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(cpy);
        cpy = e;
        pre_expression.right = cpy;
        try self.trash.append(cpy);
    }
    return .{ .prefix_expression = pre_expression };
}
