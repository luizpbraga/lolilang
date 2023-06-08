const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");

// TODO: remove null and implement parsing error msg
const Self = @This();
const PrefixParseFn = *const fn (*Self) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Self, *ast.Expression) anyerror!ast.Expression;

// TODO: better error
const ParseError = error{
    UnexpectToken,
};

pub const Precedence = enum {
    lower,
    assigne,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,
    index,

    pub fn peek(token_type: Token.TokenType) Precedence {
        return switch (token_type) {
            .@"==", .@"!=" => .equals,
            .@">", .@"<" => .lessgreater,
            .@"+", .@"-" => .sum,
            .@"/", .@"*" => .product,
            .@"(" => .call,
            .@"[" => .index,
            .@"=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
            else => .lower,
        };
    }
};

const GarbageCollector = struct {
    allocator: std.mem.Allocator,

    expression: std.ArrayList(*ast.Expression),
    identifiers: std.ArrayList([]ast.Identifier),
    statements: std.ArrayList([]ast.Statement),
    expressions: std.ArrayList([]ast.Expression),

    fn init(allocator: std.mem.Allocator) @This() {
        // TODO: use AutoHashMap(usize, pointer)
        return .{
            .allocator = allocator,
            .expression = std.ArrayList(*ast.Expression).init(allocator),
            .identifiers = std.ArrayList([]ast.Identifier).init(allocator),
            .statements = std.ArrayList([]ast.Statement).init(allocator),
            .expressions = std.ArrayList([]ast.Expression).init(allocator),
        };
    }
};

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
allocator: std.mem.Allocator,
gc: GarbageCollector,
infix_parse_fns: std.AutoHashMap(Token.TokenType, InfixParseFn),
prefix_parse_fns: std.AutoHashMap(Token.TokenType, PrefixParseFn),

pub fn new(allocator: std.mem.Allocator, lexer: *Lexer) !Self {
    var prefix_parse_fns = std.AutoHashMap(Token.TokenType, PrefixParseFn).init(allocator);
    var infix_parse_fns = std.AutoHashMap(Token.TokenType, InfixParseFn).init(allocator);

    var p = Self{
        .lexer = lexer,
        .cur_token = undefined,
        .last_token = Token{ .type = .eof, .literal = "" },
        .peek_token = undefined,
        .prefix_parse_fns = prefix_parse_fns,
        .infix_parse_fns = infix_parse_fns,
        .allocator = allocator,
        .gc = GarbageCollector.init(allocator),
    };

    p.nextToken();
    p.nextToken();

    try p.registerPrefix(.identifier, parseIdentifier);
    try p.registerPrefix(.int, parseIntegerLiteral);
    try p.registerPrefix(.true, parseBoolean);
    try p.registerPrefix(.false, parseBoolean);
    try p.registerPrefix(.@"!", parsePrefixExpression);
    try p.registerPrefix(.@"-", parsePrefixExpression);
    try p.registerPrefix(.@"(", parseGroupExpression);
    try p.registerPrefix(.@"if", parseIfExpression);
    try p.registerPrefix(.@"else", parseIfExpression);
    try p.registerPrefix(.@"fn", parseFunctionLiteral);
    try p.registerPrefix(.string, parseStringLiteral);
    try p.registerPrefix(.@"{", parseArrayLiteral);

    try p.registerInfix(.@"[", parseIndexExpression);

    try p.registerInfix(.@"+", parseInfixExpression);
    try p.registerInfix(.@"-", parseInfixExpression);
    try p.registerInfix(.@"==", parseInfixExpression);
    try p.registerInfix(.@"!=", parseInfixExpression);
    try p.registerInfix(.@"*", parseInfixExpression);
    try p.registerInfix(.@"/", parseInfixExpression);
    try p.registerInfix(.@">", parseInfixExpression);
    try p.registerInfix(.@"<", parseInfixExpression);
    try p.registerInfix(.@"(", parseCallExpression);

    try p.registerInfix(.@"=", parseAssignmentExpression);
    try p.registerInfix(.@"+=", parseAssignmentExpression);
    try p.registerInfix(.@"-=", parseAssignmentExpression);
    try p.registerInfix(.@"*=", parseAssignmentExpression);
    try p.registerInfix(.@"/=", parseAssignmentExpression);

    return p;
}

pub fn deinit(self: *Self) void {
    for (self.gc.expression.items) |f|
        self.allocator.destroy(f);

    for (self.gc.statements.items) |stmt| {
        self.allocator.free(stmt);
    }

    for (self.gc.identifiers.items) |iden|
        self.allocator.free(iden);

    for (self.gc.expressions.items) |exp|
        self.allocator.free(exp);

    self.prefix_parse_fns.deinit();
    self.infix_parse_fns.deinit();
    self.gc.expression.deinit();
    self.gc.statements.deinit();
    self.gc.identifiers.deinit();
    self.gc.expressions.deinit();
}

fn registerPrefix(self: *Self, token_type: Token.TokenType, func: PrefixParseFn) !void {
    try self.prefix_parse_fns.put(token_type, func);
}

fn registerInfix(self: *Self, token_type: Token.TokenType, func: InfixParseFn) !void {
    try self.infix_parse_fns.put(token_type, func);
}

fn nextToken(self: *Self) void {
    self.last_token = self.cur_token;
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn curTokenIs(self: *const Self, token_type: Token.TokenType) bool {
    return self.cur_token.type == token_type;
}

fn peekTokenIs(self: *const Self, token_type: Token.TokenType) bool {
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

fn peekPrecedence(self: *const Self) Precedence {
    return Precedence.peek(self.peek_token.type);
}

fn currentPrecedence(self: *const Self) Precedence {
    return Precedence.peek(self.cur_token.type);
}

// parsers fn -----------------------------------------------------------
fn parseIdentifier(self: *const Self) anyerror!ast.Expression {
    return .{ .identifier = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    } };
}

fn parseReturnStatement(self: *Self) anyerror!ast.ReturnStatement {
    var stmt = ast.ReturnStatement{
        .token = self.cur_token,
    };

    self.nextToken();

    stmt.value = (try self.parseExpression(Precedence.lower)).*;

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

fn parseAssignmentExpression(self: *Self, name: *ast.Expression) anyerror!ast.Expression {
    var stmt = ast.AssignmentExpression{
        .token = self.cur_token,
        .name = switch (name.*) {
            .identifier => |val| val,
            else => return error.UnknowExpression,
        },
        .operator = undefined,
        .value = undefined,
    };

    const op = self.cur_token;

    self.nextToken();

    stmt.operator = switch (op.type) {
        .@"=" => "=",
        .@"+=" => "+=",
        .@"-=" => "-=",
        .@"/=" => "/=",
        .@"*=" => "*=",
        else => return error.UnexpectToken,
    };

    stmt.value = try self.parseExpression(.lower);

    return .{ .assignment_expression = stmt };
}

fn parseVarStatement(self: *Self) anyerror!ast.VarStatement {
    var stmt = ast.VarStatement{
        .token = self.cur_token,
        .name = undefined,
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

    self.nextToken();

    // TODO: pointer?
    stmt.value = (try self.parseExpression(Precedence.lower)).*;

    if (self.peekTokenIs(.@";")) {
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

fn parseExpressionStatement(self: *Self) anyerror!ast.ExpressionStatement {
    var stmt = ast.ExpressionStatement{
        .token = self.cur_token,
        .expression = try self.parseExpression(Precedence.lower),
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

fn parseExpression(self: *Self, precedence: Precedence) !*ast.Expression {
    const prefix_fn = self.prefix_parse_fns.get(self.cur_token.type) orelse {
        std.log.warn("at token: {s}\n", .{self.cur_token.literal});
        return error.UnknowPrefixFn;
    };

    var left_exp = try self.allocator.create(ast.Expression);
    errdefer self.allocator.destroy(left_exp);

    left_exp.* = try prefix_fn(self);
    try self.gc.expression.append(left_exp);

    while (!self.peekTokenIs(.@";") and
        @enumToInt(precedence) < @enumToInt(self.peekPrecedence()))
    {
        const infix_fn = self.infix_parse_fns.get(self.peek_token.type) orelse return left_exp;
        self.nextToken();

        var new_left_exp = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(new_left_exp);

        new_left_exp.* = try infix_fn(self, left_exp);
        try self.gc.expression.append(new_left_exp);

        left_exp.* = new_left_exp.*;
    }

    return left_exp;
}

pub fn parseProgram(self: *Self, allocator: std.mem.Allocator) !ast.Program {
    var stmts = std.ArrayList(ast.Statement).init(allocator);
    errdefer stmts.deinit();

    var program = ast.Program{ .statements = stmts };

    var stmt: ast.Statement = undefined;
    while (self.cur_token.type != .eof) {
        stmt = try self.parseStatement();
        try program.statements.append(stmt);
        self.nextToken();
    }

    return program;
}

fn parseIntegerLiteral(self: *Self) anyerror!ast.Expression {
    return .{
        .integer_literal = ast.IntegerLiteral{
            .token = self.cur_token,
            .value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch |err| {
                std.log.err("Could not parse {s} as integer", .{self.cur_token.literal});
                return err;
            },
        },
    };
}

fn parseBoolean(self: *Self) anyerror!ast.Expression {
    return .{ .boolean = .{ .token = self.cur_token, .value = self.curTokenIs(.true) } };
}

//  BUG
fn parseGroupExpression(self: *Self) anyerror!ast.Expression {
    self.nextToken();

    var exp = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@")")) return error.MissingParentese;

    return exp.*;
}

fn parsePrefixExpression(self: *Self) anyerror!ast.Expression {
    var expression = ast.PrefixExpression{
        .operator = self.cur_token.literal,
        .token = self.cur_token,
        .right = undefined,
    };

    self.nextToken();

    expression.right = try self.parseExpression(Precedence.prefix);

    return .{ .prefix_expression = expression };
}

fn parseInfixExpression(self: *Self, left: *ast.Expression) anyerror!ast.Expression {
    var new_left = try self.allocator.create(ast.Expression);
    errdefer self.allocator.destroy(new_left);

    try self.gc.expression.append(new_left);
    new_left.* = left.*;

    var expression = ast.InfixExpression{
        .token = self.cur_token,
        .operator = self.cur_token.literal,
        .left = new_left,
        .right = undefined,
    };

    var precedence = self.currentPrecedence();

    self.nextToken();

    expression.right = try self.parseExpression(precedence);

    return .{ .infix_expression = expression };
}

fn parseIfExpression(self: *Self) anyerror!ast.Expression {
    var expression = ast.IfExpression{
        .token = self.cur_token,
        .condition = undefined,
        .consequence = undefined,
    };

    // if (!self.expectPeek(.@"(")) return error.MissingParentese;

    self.nextToken();

    expression.condition = try self.parseExpression(Precedence.lower);

    // if (!self.expectPeek(.@")")) return error.MissingParentese;
    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    expression.consequence = try self.parseBlockStatement();

    if (self.peekTokenIs(.@"else")) {
        self.nextToken();
        if (!self.expectPeek(.@"{")) return error.MissingBrance;
        expression.alternative = try self.parseBlockStatement();
    }

    return .{ .if_expression = expression };
}

fn parseBlockStatement(self: *Self) anyerror!ast.BlockStatement {
    var stmts = std.ArrayList(ast.Statement).init(self.allocator);
    errdefer stmts.deinit();

    var block = ast.BlockStatement{
        .token = self.cur_token,
        .statements = undefined,
    };

    self.nextToken();

    while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        var stmt = try self.parseStatement();
        try stmts.append(stmt);
        self.nextToken();
    }

    var stmts_owner = try stmts.toOwnedSlice();

    try self.gc.statements.append(stmts_owner);

    block.statements = stmts_owner;

    return block;
}

fn parseFunctionLiteral(self: *Self) anyerror!ast.Expression {
    var lit = ast.FunctionLiteral{
        .token = self.cur_token,
        .parameters = undefined,
        .body = undefined,
    };

    if (!self.expectPeek(.@"(")) return error.MissingParentese;

    lit.parameters = try self.parseFunctionParameters();

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    lit.body = try self.parseBlockStatement();

    return .{ .function_literal = lit };
}

fn parseFunctionParameters(self: *Self) anyerror![]ast.Identifier {
    var indentifiers = std.ArrayList(ast.Identifier).init(self.allocator);
    errdefer indentifiers.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        return try indentifiers.toOwnedSlice();
    }

    self.nextToken();

    // var ident = try self.allocator.create(ast.Identifier);
    var ident = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    try indentifiers.append(ident);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        // var ident2 = try self.allocator.create(ast.Identifier);
        var ident2 = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };

        try indentifiers.append(ident2);
    }

    if (!self.expectPeek(.@")"))
        return error.MissingParentese;

    var ident_owner = try indentifiers.toOwnedSlice();

    try self.gc.identifiers.append(ident_owner);

    return ident_owner;
}

fn parseCallExpression(self: *Self, func: *ast.Expression) anyerror!ast.Expression {
    var func_ = try self.allocator.create(ast.Expression);
    func_.* = func.*;
    try self.gc.expression.append(func_);

    var call = .{
        .call_expression = .{
            .token = self.cur_token,
            .function = func_,
            // TODO: use self.parseExpressionList(.@")")
            .arguments = try self.parseCallArguments(),
        },
    };

    return call;
}

fn parseCallArguments(self: *Self) anyerror![]ast.Expression {
    var args = std.ArrayList(ast.Expression).init(self.allocator);
    errdefer args.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        // TODO: possible segfault
        return try args.toOwnedSlice();
    }

    self.nextToken();
    var exp = try self.parseExpression(Precedence.lower);
    var exp_ = exp.*;
    try args.append(exp_);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        var exp2 = try self.parseExpression(Precedence.lower);
        var exp2_ = exp2.*;
        try args.append(exp2_);
    }

    if (!self.expectPeek(.@")")) return error.MissingRightParenteses;

    var args_owned = try args.toOwnedSlice();
    try self.gc.expressions.append(args_owned);

    return args_owned;
}

pub fn parseStringLiteral(self: *Self) anyerror!ast.Expression {
    return .{
        .string_literal = .{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        },
    };
}

pub fn parseArrayLiteral(self: *Self) anyerror!ast.Expression {
    return .{
        .array_literal = .{
            .token = self.cur_token,
            .elements = try self.parseExpressionList(.@"}"),
        },
    };
}
pub fn parseExpressionList(self: *Self, end: Token.TokenType) anyerror![]ast.Expression {
    var list = std.ArrayList(ast.Expression).init(self.allocator);
    errdefer list.deinit();

    if (self.peekTokenIs(end)) {
        self.nextToken();
        return &.{};
    }

    self.nextToken();
    var exp = try self.parseExpression(Precedence.lower);
    try list.append(exp.*);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();
        var exp2 = try self.parseExpression(Precedence.lower);
        try list.append(exp2.*);
    }

    if (!self.expectPeek(end)) {
        return error.MissingRightBrace;
    }

    var list_owned = try list.toOwnedSlice();
    try self.gc.expressions.append(list_owned);

    return list_owned;
}

pub fn parseIndexExpression(self: *Self, left: *ast.Expression) anyerror!ast.Expression {
    var new_left = try self.allocator.create(ast.Expression);
    errdefer self.allocator.destroy(new_left);

    try self.gc.expression.append(new_left);
    new_left.* = left.*;

    var exp = ast.IndexExpression{
        .token = self.cur_token,
        .left = new_left,
        .index = undefined,
    };

    self.nextToken();

    exp.index = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@"]"))
        return error.MissingRightBracket;

    return .{ .index_expression = exp };
}
