/// Parser
const Parser = @This();

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
arena: std.heap.ArenaAllocator,
infix_parse_fns: std.AutoHashMap(Token.TokenType, InfixParseFn),
prefix_parse_fns: std.AutoHashMap(Token.TokenType, PrefixParseFn),

// TODO: remove null and implement parsing error msg
const PrefixParseFn = *const fn (*Parser) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, *ast.Expression) anyerror!ast.Expression;

const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");

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
            .@"[", .@"." => .index,
            .@"=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
            else => .lower,
        };
    }
};

pub fn new(child_alloc: std.mem.Allocator, lexer: *Lexer) !Parser {
    var arena = std.heap.ArenaAllocator.init(child_alloc);

    var p = Parser{
        .arena = arena,
        .lexer = lexer,
        .cur_token = undefined,
        .last_token = Token{ .type = .eof, .literal = "" },
        .peek_token = undefined,
        .infix_parse_fns = undefined,
        .prefix_parse_fns = undefined,
    };

    const allocator = p.arena.allocator();

    p.infix_parse_fns = std.AutoHashMap(Token.TokenType, InfixParseFn).init(allocator);
    p.prefix_parse_fns = std.AutoHashMap(Token.TokenType, PrefixParseFn).init(allocator);

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

    try p.registerPrefix(.@"for", parseForLoop);
    try p.registerPrefix(.@"switch", parseSwitch);
    try p.registerPrefix(.@"[", parseArrayLiteral);

    try p.registerPrefix(.@"else", parseIfExpression);
    try p.registerPrefix(.@"fn", parseFunctionLiteral);
    // try p.registerPrefix(.@"enum", parseEnumLiteral);
    try p.registerPrefix(.string, parseStringLiteral);
    try p.registerPrefix(.@"{", parseHashLiteral);
    try p.registerInfix(.@"[", parseIndexExpression);

    try p.registerInfix(.@"+", parseInfixExpression);
    try p.registerInfix(.@"-", parseInfixExpression);
    try p.registerInfix(.@":", parseInfixExpression);
    try p.registerInfix(.@"==", parseInfixExpression);
    try p.registerInfix(.@"!=", parseInfixExpression);
    try p.registerInfix(.@"*", parseInfixExpression);
    try p.registerInfix(.@"/", parseInfixExpression);
    try p.registerInfix(.@">", parseInfixExpression);
    try p.registerInfix(.@"<", parseInfixExpression);
    try p.registerInfix(.@"(", parseCallExpression);
    try p.registerInfix(.@".", parseMethodExpression);

    try p.registerInfix(.@"=", parseAssignmentExpression);
    try p.registerInfix(.@"+=", parseAssignmentExpression);
    try p.registerInfix(.@"-=", parseAssignmentExpression);
    try p.registerInfix(.@"*=", parseAssignmentExpression);
    try p.registerInfix(.@"/=", parseAssignmentExpression);

    return p;
}

pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}

fn registerPrefix(self: *Parser, token_type: Token.TokenType, func: PrefixParseFn) !void {
    try self.prefix_parse_fns.put(token_type, func);
}

fn registerInfix(self: *Parser, token_type: Token.TokenType, func: InfixParseFn) !void {
    try self.infix_parse_fns.put(token_type, func);
}

fn nextToken(self: *Parser) void {
    self.last_token = self.cur_token;
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn curTokenIs(self: *const Parser, token_type: Token.TokenType) bool {
    return self.cur_token.type == token_type;
}

fn peekTokenIs(self: *const Parser, token_type: Token.TokenType) bool {
    return self.peek_token.type == token_type;
}

/// match and eat the token (if true)
fn expectPeek(self: *Parser, token_type: Token.TokenType) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    } else {
        return false;
    }
}

fn peekPrecedence(self: *const Parser) Precedence {
    return Precedence.peek(self.peek_token.type);
}

fn currentPrecedence(self: *const Parser) Precedence {
    return Precedence.peek(self.cur_token.type);
}

// parsers fn -----------------------------------------------------------
fn parseIdentifier(self: *const Parser) anyerror!ast.Expression {
    return .{ .identifier = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    } };
}

fn parseReturnStatement(self: *Parser) anyerror!ast.ReturnStatement {
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

fn parseBreakStatement(self: *Parser) anyerror!ast.ReturnStatement {
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

fn parseAssignmentExpression(self: *Parser, name: *ast.Expression) anyerror!ast.Expression {
    const allocator = self.arena.allocator();
    var left_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(left_exp);

    left_exp.* = name.*;
    // try self.gc.expression.append(left_exp);

    var stmt = ast.AssignmentExpression{
        .token = self.cur_token,
        .name = left_exp,
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

fn parseConstStatement(self: *Parser) anyerror!ast.ConstStatement {
    var stmt = ast.ConstStatement{
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
        std.log.err("Expect Assignment Token (=), found '{s}' (hint: U cannot use numbers in var names...)", .{self.cur_token.literal});
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

fn parseVarBlockStatement(self: *Parser) anyerror!ast.VarBlockStatement {
    var stmt = ast.VarBlockStatement{
        .token = self.last_token,
        .vars_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars = std.ArrayList(ast.VarStatement).init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@"}") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        var var_stmt = ast.VarStatement{
            .token = stmt.token,
            .name = ident.identifier,
            .value = exp.*,
        };

        try vars.append(var_stmt);

        self.nextToken();
    }

    var vars_owned = try vars.toOwnedSlice();

    // try self.gc.vars_decl.append(vars_owned);
    stmt.vars_decl = vars_owned;

    return stmt;
}

fn parseConstBlockStatement(self: *Parser) anyerror!ast.ConstBlockStatement {
    var stmt = ast.ConstBlockStatement{
        .token = self.last_token,
        .const_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars = std.ArrayList(ast.ConstStatement).init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@"}") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        var var_stmt = ast.ConstStatement{
            .token = stmt.token,
            .name = ident.identifier,
            .value = exp.*,
        };

        try vars.append(var_stmt);

        self.nextToken();
    }

    var vars_owned = try vars.toOwnedSlice();

    // try self.gc.const_decl.append(vars_owned);
    stmt.const_decl = vars_owned;

    return stmt;
}

fn parseVarStatement(self: *Parser) anyerror!ast.VarStatement {
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

fn parseStatement(self: *Parser) !ast.Statement {
    return switch (self.cur_token.type) {
        .@"var" => if (!self.expectPeek(.@"{"))
            .{ .var_statement = try self.parseVarStatement() }
        else
            .{ .var_block_statement = try self.parseVarBlockStatement() },

        .@"const" => if (!self.expectPeek(.@"{"))
            .{ .const_statement = try self.parseConstStatement() }
        else
            .{ .const_block_statement = try self.parseConstBlockStatement() },

        .@"return" => .{ .return_statement = try self.parseReturnStatement() },
        else => .{ .expression_statement = try self.parseExpressionStatement() },
    };
}

fn parseExpressionStatement(self: *Parser) anyerror!ast.ExpressionStatement {
    var stmt = ast.ExpressionStatement{
        .token = self.cur_token,
        .expression = try self.parseExpression(Precedence.lower),
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return stmt;
}

fn parseExpression(self: *Parser, precedence: Precedence) !*ast.Expression {
    const prefix_fn = self.prefix_parse_fns.get(self.cur_token.type) orelse {
        std.log.warn("at token: {s}\n", .{self.cur_token.literal});
        return error.UnknowPrefixFn;
    };

    const allocator = self.arena.allocator();

    var left_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(left_exp);

    left_exp.* = try prefix_fn(self);
    // try self.gc.expression.append(left_exp);

    while (@intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        const infix_fn = self.infix_parse_fns.get(self.peek_token.type) orelse return left_exp;
        self.nextToken();

        var new_left_exp = try allocator.create(ast.Expression);
        errdefer allocator.destroy(new_left_exp);

        new_left_exp.* = try infix_fn(self, left_exp);
        // try self.gc.expression.append(new_left_exp);

        left_exp.* = new_left_exp.*;
    }

    return left_exp;
}

pub fn parseProgram(self: *Parser) !ast.Program {
    const allocator = self.arena.allocator();

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

fn parseIntegerLiteral(self: *Parser) anyerror!ast.Expression {
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

fn parseBoolean(self: *Parser) anyerror!ast.Expression {
    return .{ .boolean = .{ .token = self.cur_token, .value = self.curTokenIs(.true) } };
}

//  BUG
fn parseGroupExpression(self: *Parser) anyerror!ast.Expression {
    self.nextToken();

    var exp = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@")")) return error.MissingParentese;

    return exp.*;
}

fn parsePrefixExpression(self: *Parser) anyerror!ast.Expression {
    var expression = ast.PrefixExpression{
        .operator = self.cur_token.literal,
        .token = self.cur_token,
        .right = undefined,
    };

    self.nextToken();

    expression.right = try self.parseExpression(Precedence.prefix);

    return .{ .prefix_expression = expression };
}

fn parseInfixExpression(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    const allocator = self.arena.allocator();

    var new_left = try allocator.create(ast.Expression);
    errdefer allocator.destroy(new_left);

    // try self.gc.expression.append(new_left);
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

fn parseIfExpression(self: *Parser) anyerror!ast.Expression {
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

fn parseBlockStatement(self: *Parser) anyerror!ast.BlockStatement {
    const allocator = self.arena.allocator();

    var stmts = std.ArrayList(ast.Statement).init(allocator);
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

    // try self.gc.statements.append(stmts_owner);

    block.statements = stmts_owner;

    return block;
}

// fn parseEnumLiteral(self: *Self) anyerror!ast.Expression {
//     var enu = ast.EnumLiteral{
//         .token = ast.cur_token,
//         .fields = undefined, // hash(ident, value)
//     };

//     if (!self.expectPeek(.@"{")) return error.MissingBrance;

//     var i: usize = 0;
//     while (!self.expectPeek(.@"}")) {
//         var ident_exp = try self.parseIdentifier();

//         if (ident_exp != .identifier) return error.NotAIdent;

//         try enu.fields.put(ident_exp.identifier, i);
//         i += 1;

//         self.nextToken();
//     }

//     return .{ .enum_literal = enu };
// }

fn parseFunctionLiteral(self: *Parser) anyerror!ast.Expression {
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

fn parseFunctionParameters(self: *Parser) anyerror![]ast.Identifier {
    const allocator = self.arena.allocator();

    var indentifiers = std.ArrayList(ast.Identifier).init(allocator);
    errdefer indentifiers.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        return try indentifiers.toOwnedSlice();
    }

    self.nextToken();

    var ident = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    try indentifiers.append(ident);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        var ident2 = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };

        try indentifiers.append(ident2);
    }

    if (!self.expectPeek(.@")"))
        return error.MissingParentese;

    var ident_owner = try indentifiers.toOwnedSlice();

    // try self.gc.identifiers.append(ident_owner);

    return ident_owner;
}

fn parseMethodExpression(self: *Parser, exp: *ast.Expression) anyerror!ast.Expression {
    const allocator = self.arena.allocator();

    // TODO: parse the Expression
    var caller = try allocator.create(ast.Expression);
    caller.* = exp.*;
    // try self.gc.expression.append(caller);

    var method_exp = ast.MethodExpression{
        .token = self.cur_token,
        .caller = caller,
        .method = undefined,
    };

    self.nextToken();

    var exp_mathod = try self.parseIdentifier();

    if (exp_mathod != .identifier) return error.ExpectIdentifier;

    method_exp.method = exp_mathod.identifier;

    // self.nextToken();

    return .{ .method_expression = method_exp };
}

fn parseCallExpression(self: *Parser, func: *ast.Expression) anyerror!ast.Expression {
    const allocator = self.arena.allocator();
    var func_ = try allocator.create(ast.Expression);

    func_.* = func.*;
    // try self.gc.expression.append(func_);

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

fn parseCallArguments(self: *Parser) anyerror![]ast.Expression {
    const allocator = self.arena.allocator();
    var args = std.ArrayList(ast.Expression).init(allocator);
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

    return args_owned;
}

pub fn parseStringLiteral(self: *Parser) anyerror!ast.Expression {
    return .{
        .string_literal = .{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        },
    };
}

pub fn parseArrayLiteral(self: *Parser) anyerror!ast.Expression {
    const token = self.cur_token;

    if (self.peekTokenIs(.@"]")) {
        self.nextToken();
        return .{
            .array_literal = .{ .token = token, .elements = &.{} },
        };
    }

    const allocator = self.arena.allocator();
    var elements = std.ArrayList(ast.Expression).init(allocator);
    errdefer elements.deinit();

    self.nextToken();
    var element1 = try self.parseExpression(.lower);
    try elements.append(element1.*);

    while (self.peekTokenIs(.@",")) {
        self.nextToken(); // ,
        self.nextToken(); // next_element

        var element_n = try self.parseExpression(.lower);
        try elements.append(element_n.*);
    }

    if (!self.expectPeek(.@"]")) return error.UnexpectToken;

    return .{
        .array_literal = .{ .token = token, .elements = try elements.toOwnedSlice() },
    };
}

// var hash = {1:1, 2:2, 3:3}
pub fn parseHashLiteral(self: *Parser) anyerror!ast.Expression {
    const token = self.cur_token;
    const allocator = self.arena.allocator();

    var hash = std.AutoHashMap(*ast.Expression, *ast.Expression).init(allocator);
    errdefer hash.deinit();

    while (!self.peekTokenIs(.@"}")) {
        self.nextToken();

        var key = try self.parseExpression(.lower);

        if (!self.expectPeek(.@":")) {
            std.log.warn("syntax error: expect ':'\n", .{});
            return error.MissingValueInHash;
        }

        self.nextToken();

        var val = try self.parseExpression(.lower);

        try hash.put(key, val);

        if (!self.peekTokenIs(.@"}") and !self.expectPeek(.@",")) {
            std.log.warn("syntax error: expect ',' or '}}'\n", .{});
            return error.UnexpectToken;
        }
    }

    if (!self.expectPeek(.@"}")) {
        std.log.warn("syntax error: expect '}}'\n", .{});
        return error.MissingRightBrace;
    }

    return .{ .hash_literal = .{ .token = token, .elements = hash } };
}

// (...)
// pub fn parseExpressionList(self: *Parser, exp1: *ast.Expression, end: Token.TokenType) anyerror![]ast.Expression {
//     const allocator = self.arena.allocator();
//     var list = std.ArrayList(ast.Expression).init(allocator);
//     errdefer list.deinit();
//
//     try list.append(exp1.*);
//
//     var exp = try self.parseExpression(Precedence.lower);
//     try list.append(exp.*);
//
//     while (self.peekTokenIs(.@",")) {
//         self.nextToken();
//         self.nextToken();
//         var exp2 = try self.parseExpression(Precedence.lower);
//         try list.append(exp2.*);
//     }
//
//     if (self.expectPeek(.@":"))
//         return error.NotAHash;
//
//     if (!self.expectPeek(end)) {
//         return error.MissingRightBrace;
//     }
//
//     var list_owned = try list.toOwnedSlice();
//     // try self.gc.expressions.append(list_owned);
//
//     return list_owned;
// }

// v[exp]
pub fn parseIndexExpression(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    const allocator = self.arena.allocator();
    var new_left = try allocator.create(ast.Expression);
    errdefer allocator.destroy(new_left);

    // try self.gc.expression.append(new_left);
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

// var y = switch {
//      exp1 => [block1, exp],
//      exp2 => [block2, exp],
//      else => [block3, exp],
//}
pub fn parseSwitch(self: *Parser) anyerror!ast.Expression {
    var swi = ast.SwitchExpression{
        .token = self.cur_token,
        .value = undefined,
        .choices = undefined,
    };

    self.nextToken();

    swi.value = try self.parseExpression(.lower);

    const allocator = self.arena.allocator();

    var choices = std.ArrayList(ast.SwitchChoices).init(allocator);
    errdefer choices.deinit();

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    self.nextToken();

    while (self.cur_token.type != .@"}") {
        if (self.curTokenIs(.eof)) return error.Bruuuuuuuuuuuh;

        var stop = if (self.cur_token.type == .@"else") true else false;

        var exp = if (stop) b: {
            var v = try self.parseIdentifier();
            var cpy = try allocator.create(ast.Expression);
            errdefer allocator.destroy(cpy);

            // try self.gc.expression.append(cpy);
            cpy.* = v;

            break :b cpy;
        } else try self.parseExpression(.lower);

        if (!self.expectPeek(.@"=>")) return error.UnexpectToken;

        var sc = ast.SwitchChoices{
            .token = self.cur_token,
            .exp = exp,
            .block = undefined,
        };

        if (!self.expectPeek(.@"{")) return error.MissingBlockBrace;

        sc.block = try self.parseBlockStatement();

        self.nextToken();

        if (!stop)
            if (self.cur_token.type != .@",")
                return error.MissingSemiColon;

        if (!stop)
            self.nextToken();

        try choices.append(sc);

        if (stop) break;
    }

    var choice_owned = try choices.toOwnedSlice();
    // try self.gc.choices.append(choice_owned);
    swi.choices = choice_owned;

    return .{ .switch_expression = swi };
}

// for true { } or for idx, val in list {}
pub fn parseForLoop(self: *Parser) anyerror!ast.Expression {
    var fl = ast.ForLoopExpression{
        .token = self.cur_token,
        .condition = undefined,
        .consequence = undefined,
    };

    if (self.expectPeek(.@"{")) {
        fl.consequence = try self.parseBlockStatement();
        return .{ .forloop_expression = fl };
    }

    self.nextToken();

    fl.condition = try self.parseExpression(.lower);

    // for range expression paeser: make it  a function
    if (fl.condition.* == .identifier and (self.expectPeek(.in) or self.expectPeek(.@","))) {
        var flr = ast.ForLoopRangeExpression{
            .token = fl.token,
            .ident = fl.condition.identifier.value,
            .body = undefined,
            .iterable = undefined,
        };

        if (self.cur_token.type == .in) {
            self.nextToken();
            var exp = try self.parseExpression(.lower);
            flr.iterable = exp;
        } else if (self.cur_token.type == .@",") {
            self.nextToken();
            var index = try self.parseIdentifier();
            flr.index = index.identifier.value;
            if (self.expectPeek(.in)) {
                self.nextToken();
                var exp = try self.parseExpression(.lower);
                flr.iterable = exp;
            }
        }

        if (!self.expectPeek(.@"{")) return error.MissingBrance;
        flr.body = try self.parseBlockStatement();
        return .{ .forloop_range_expression = flr };
    }

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    fl.consequence = try self.parseBlockStatement();

    return .{ .forloop_expression = fl };
}
