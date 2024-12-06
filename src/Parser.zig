const Parser = @This();

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
arena: std.heap.ArenaAllocator,
infix_fns: std.EnumMap(Token.Type, InfixParseFn),
prefix_fns: std.EnumMap(Token.Type, PrefixParseFn),

pub const Precedence = enum {
    lower,
    cond,
    assigne,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,
    index,

    pub fn peek(token_type: Token.Type) Precedence {
        return switch (token_type) {
            .@"or", .@"and" => .cond,
            .@"==", .@"!=" => .equals,
            .@">", .@"<" => .lessgreater,
            .@"+", .@"-" => .sum,
            .@"/", .@"*" => .product,
            .@"(" => .call,
            .@"!" => .prefix,
            .@"[", .@".", .@".." => .index,
            .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
            else => .lower,
        };
    }
};

pub fn new(child_alloc: std.mem.Allocator, lexer: *Lexer) Parser {
    const arena: std.heap.ArenaAllocator = .init(child_alloc);

    var parser: Parser = .{
        .arena = arena,
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .last_token = .{ .type = .eof, .literal = "" },
        .infix_fns = .init(.{}),
        .prefix_fns = .init(.{}),
    };

    parser.nextToken();
    parser.nextToken();

    parser.prefix_fns.put(.identifier, parseIdentifier);
    parser.prefix_fns.put(.int, parseIntegerLiteral);
    parser.prefix_fns.put(.float, parseFloatLiteral);
    parser.prefix_fns.put(.true, parseBoolean);
    parser.prefix_fns.put(.false, parseBoolean);
    parser.prefix_fns.put(.null, parseNull);
    parser.prefix_fns.put(.@"[", parseArrayLiteral);
    parser.prefix_fns.put(.@"{", parseHashLiteral);
    parser.prefix_fns.put(.func, parseFunctionLiteral);
    parser.prefix_fns.put(.string, parseStringLiteral);
    parser.prefix_fns.put(.@"if", parseIfExpression);
    // parser.prefix_fns.put(.@"for", parseForLoop);
    // parser.prefix_fns.put(.@"switch", parseSwitchExpression);
    // parser.prefix_fns.put(.@"enum", parseEnumLiteral);
    parser.prefix_fns.put(.@"else", parseIfExpression);
    parser.prefix_fns.put(.@"!", parsePrefixExpression);
    parser.prefix_fns.put(.@"-", parsePrefixExpression);
    parser.prefix_fns.put(.@"(", parseGroupExpression);

    parser.infix_fns.put(.@"[", parseIndexExpression);
    // parser.infix_fns.put(.@"..", parseRangeExpression);
    parser.infix_fns.put(.@".", parseMethodExpression);
    parser.infix_fns.put(.@"(", parseCallExpression);
    parser.infix_fns.put(.@"+", parseInfixExpression);
    parser.infix_fns.put(.@"-", parseInfixExpression);
    parser.infix_fns.put(.@":", parseInfixExpression);
    parser.infix_fns.put(.@"==", parseInfixExpression);
    parser.infix_fns.put(.@"!=", parseInfixExpression);
    parser.infix_fns.put(.@"*", parseInfixExpression);
    parser.infix_fns.put(.@"or", parseInfixExpression);
    parser.infix_fns.put(.@"and", parseInfixExpression);
    parser.infix_fns.put(.@"/", parseInfixExpression);
    parser.infix_fns.put(.@">", parseInfixExpression);
    parser.infix_fns.put(.@"<", parseInfixExpression);
    parser.infix_fns.put(.@"=", parseAssignmentExpression);
    parser.infix_fns.put(.@":=", parseAssignmentExpression);
    parser.infix_fns.put(.@"+=", parseAssignmentExpression);
    parser.infix_fns.put(.@"-=", parseAssignmentExpression);
    parser.infix_fns.put(.@"*=", parseAssignmentExpression);
    parser.infix_fns.put(.@"/=", parseAssignmentExpression);

    return parser;
}

pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}

fn nextToken(self: *Parser) void {
    self.last_token = self.cur_token;
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn curTokenIs(self: *const Parser, token_type: Token.Type) bool {
    return self.cur_token.type == token_type;
}

fn peekTokenIs(self: *const Parser, token_type: Token.Type) bool {
    return self.peek_token.type == token_type;
}

/// match and eat the token (if true)
fn expectPeek(self: *Parser, token_type: Token.Type) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    } else {
        return false;
    }
}

fn peekPrecedence(self: *const Parser) Precedence {
    return .peek(self.peek_token.type);
}

fn currentPrecedence(self: *const Parser) Precedence {
    return .peek(self.cur_token.type);
}

// parsers fn -----------------------------------------------------------
fn parseIdentifier(self: *const Parser) anyerror!ast.Expression {
    return .{
        .identifier = .{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        },
    };
}

fn parseReturnStatement(self: *Parser) anyerror!ast.Statement {
    var return_stmt: ast.ReturnStatement = .{
        .token = self.cur_token,
        .value = undefined,
    };

    self.nextToken();

    return_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .return_statement = return_stmt };
}

fn parseBreakStatement(self: *Parser) anyerror!ast.Statement {
    var break_stmt: ast.BreakStatement = .{
        .token = self.cur_token,
        .value = undefined,
    };

    self.nextToken();

    break_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .break_statement = break_stmt };
}

fn parseAssignmentExpression(self: *Parser, name: *ast.Expression) anyerror!ast.Expression {
    var stmt: ast.AssignmentExpression = .{
        .token = self.cur_token,
        .name = name,
        .operator = undefined,
        .value = undefined,
    };

    const op = self.cur_token;

    self.nextToken();

    stmt.operator = switch (op.type) {
        .@"=" => "=",
        .@":=" => ":=",
        .@"+=" => "+=",
        .@"-=" => "-=",
        .@"/=" => "/=",
        .@"*=" => "*=",
        else => return error.UnexpectedToken,
    };

    stmt.value = try self.parseExpression(.lower);

    return .{ .assignment_expression = stmt };
}

pub fn parseDeferStatement(self: *Parser) anyerror!ast.Statement {
    var defer_stmt: ast.DeferStatement = .{
        .token = self.cur_token,
        .body = undefined,
    };

    if (!self.expectPeek(.@"{")) {
        return error.ExpectSomeConditionOrIdentifier;
    }

    defer_stmt.body = try self.parseBlockStatement();

    return .{ .defer_statement = defer_stmt };
}

fn parseConstStatement(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .const_block_statement = try self.parseConstBlockStatement(tk) };

    var const_stmt: ast.ConstStatement = .{
        .token = tk,
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
        return error.UnexpectedToken;
    }

    const_stmt.name = .{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        std.log.err("Expect Assignment Token (=), found '{s}' (hint: U cannot use numbers in var names...)", .{self.cur_token.literal});
        return error.UnexpectedToken;
    }

    self.nextToken();

    // TODO: pointer?
    const_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .const_statement = const_stmt };
}

fn parseVarBlockStatement(self: *Parser, tk: Token) anyerror!ast.VarBlockStatement {
    var stmt: ast.VarBlockStatement = .{
        .token = tk,
        .vars_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.VarStatement) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        const var_stmt = ast.VarStatement{
            .token = stmt.token,
            .name = ident.identifier,
            .value = exp,
        };

        try vars.append(var_stmt);

        self.nextToken();
    }

    const vars_owned = try vars.toOwnedSlice();

    stmt.vars_decl = vars_owned;

    return stmt;
}

fn parseConstBlockStatement(self: *Parser, tk: Token) anyerror!ast.ConstBlockStatement {
    var stmt: ast.ConstBlockStatement = .{
        .token = tk,
        .const_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.ConstStatement) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        const var_stmt: ast.ConstStatement = .{
            .token = stmt.token,
            .name = ident.identifier,
            .value = exp,
        };

        try vars.append(var_stmt);

        self.nextToken();
    }

    const vars_owned = try vars.toOwnedSlice();

    stmt.const_decl = vars_owned;

    return stmt;
}

fn parseVarStatement(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .var_block_statement = try self.parseVarBlockStatement(tk) };

    var var_stmt: ast.VarStatement = .{
        .token = tk,
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
        return error.UnexpectedToken;
    }

    var_stmt.name = .{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        const var_type = try self.parseExpression(.lower);
        var_stmt.type = var_type;

        self.nextToken();

        if (!self.expectPeek(.@"=")) {
            var_stmt.value.* = try self.parseNull();

            if (self.peekTokenIs(.@";")) {
                self.nextToken();
            }

            return .{ .var_statement = var_stmt };
        }
    }

    self.nextToken();

    // TODO: pointer?
    var_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .var_statement = var_stmt };
}

fn parseStatement(self: *Parser) !ast.Statement {
    return switch (self.cur_token.type) {
        .@"var" => try self.parseVarStatement(),

        .@"const" => try self.parseConstStatement(),

        .@"return" => try self.parseReturnStatement(),

        .@"break" => try self.parseBreakStatement(),

        // .func => try self.parseFunctionStatement(),

        .@"defer" => try self.parseDeferStatement(),

        .@"{" => .{ .block_statement = try self.parseBlockStatement() },

        else => try self.parseExpressionStatement(),
    };
}

fn parseExpressionStatement(self: *Parser) anyerror!ast.Statement {
    const exp_stmt: ast.ExpressionStatement = .{
        .token = self.cur_token,
        .expression = try self.parseExpression(.lower),
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .expression_statement = exp_stmt };
}

fn parseType(self: *Parser) anyerror!*ast.Expression {
    const value = try self.parseExpression(.lower);
    return .{ .type = .{ .type = value.identifier } };
}

fn parseExpression(self: *Parser, precedence: Precedence) !*ast.Expression {
    const prefixFn = self.prefix_fns.get(self.cur_token.type) orelse {
        std.log.warn("at token: {s}\n", .{self.cur_token.literal});
        return error.UnknowPrefixFn;
    };

    const allocator = self.arena.allocator();
    const left_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(left_exp);
    left_exp.* = try prefixFn(self);

    while (@intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        const infixFn = self.infix_fns.get(self.peek_token.type) orelse return left_exp;
        self.nextToken();
        const old_left_exp = try allocator.create(ast.Expression);
        errdefer allocator.destroy(old_left_exp);
        old_left_exp.* = left_exp.*;
        left_exp.* = try infixFn(self, old_left_exp);
    }

    return left_exp;
}

pub fn parseProgram(self: *Parser) !ast.Program {
    const allocator = self.arena.allocator();

    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();

    var program: ast.Program = .{ .statements = stmts };

    var stmt: ast.Statement = undefined;
    while (self.cur_token.type != .eof) {
        stmt = try self.parseStatement();
        try program.statements.append(stmt);
        self.nextToken();
    }

    return program;
}

fn parseFloatLiteral(self: *Parser) anyerror!ast.Expression {
    return .{
        .float_literal = .{
            .token = self.cur_token,
            .value = std.fmt.parseFloat(f64, self.cur_token.literal) catch |err| {
                std.log.err("Could not parse {s} as float", .{self.cur_token.literal});
                return err;
            },
        },
    };
}

fn parseIntegerLiteral(self: *Parser) anyerror!ast.Expression {
    return .{
        .integer_literal = .{
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

fn parseNull(_: *Parser) anyerror!ast.Expression {
    return .null_literal;
}

//  BUG
fn parseGroupExpression(self: *Parser) anyerror!ast.Expression {
    self.nextToken();

    const exp = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@")")) return error.MissingParentese;

    return exp.*;
}

fn parsePrefixExpression(self: *Parser) anyerror!ast.Expression {
    var expression: ast.PrefixExpression = .{
        .operator = self.cur_token.literal,
        .token = self.cur_token,
        .right = undefined,
    };

    self.nextToken();

    expression.right = try self.parseExpression(.prefix);

    return .{ .prefix_expression = expression };
}

fn parseInfixExpression(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    var expression = ast.InfixExpression{
        .token = self.cur_token,
        .operator = self.cur_token.literal,
        .left = left,
        .right = undefined,
    };

    const precedence = self.currentPrecedence();

    self.nextToken();

    expression.right = try self.parseExpression(precedence);

    return .{ .infix_expression = expression };
}

fn parseIfExpression(self: *Parser) anyerror!ast.Expression {
    var expression: ast.IfExpression = .{
        .token = self.cur_token,
        .condition = undefined,
        .consequence = undefined,
    };

    self.nextToken();

    expression.condition = try self.parseExpression(Precedence.lower);

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

    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();

    var block: ast.BlockStatement = .{
        .token = self.cur_token, //{
        .statements = undefined,
    };

    self.nextToken();

    while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        const stmt = try self.parseStatement();
        try stmts.append(stmt);
        self.nextToken();
    }

    const stmts_owner = try stmts.toOwnedSlice();

    block.statements = stmts_owner;

    return block;
}

// fn parseFunctionStatement(self: *Parser) anyerror!ast.Statement {
//     var func_stmt: ast.FunctionStatement = .{
//         .token = self.cur_token,
//         .func = undefined,
//         .name = undefined,
//     };
//
//     if (!self.expectPeek(.identifier)) {
//         std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
//         return error.UnexpectedToken;
//     }
//
//     func_stmt.name = .{
//         .token = self.cur_token,
//         .value = self.cur_token.literal,
//     };
//
//     func_stmt.func = try self.parseFunctionLiteral();
//
//     return .{ .function_statement = func_stmt };
// }

fn parseFunctionLiteral(self: *Parser) anyerror!ast.Expression {
    var lit: ast.FunctionLiteral = .{
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

    var indentifiers: std.ArrayList(ast.Identifier) = .init(allocator);
    errdefer indentifiers.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        return try indentifiers.toOwnedSlice();
    }

    self.nextToken();

    const ident: ast.Identifier = .{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    try indentifiers.append(ident);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const ident2: ast.Identifier = .{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };

        try indentifiers.append(ident2);
    }

    if (!self.expectPeek(.@")"))
        return error.MissingParentese;

    const ident_owner = try indentifiers.toOwnedSlice();

    return ident_owner;
}

fn parseMethodExpression(self: *Parser, caller: *ast.Expression) anyerror!ast.Expression {
    var method_exp: ast.MethodExpression = .{
        .token = self.cur_token,
        .caller = caller,
        .method = undefined,
    };

    self.nextToken();

    const exp_mathod = try self.parseIdentifier();

    if (exp_mathod != .identifier) return error.ExpectIdentifier;

    method_exp.method = exp_mathod.identifier;

    return .{ .method_expression = method_exp };
}

fn parseCallExpression(self: *Parser, func: *ast.Expression) anyerror!ast.Expression {
    return .{
        .call_expression = .{
            .token = self.cur_token,
            .function = func,
            // TODO: use self.parseExpressionList(.@")")
            .arguments = try self.parseCallArguments(),
        },
    };
}

fn parseCallArguments(self: *Parser) anyerror![]*ast.Expression {
    const allocator = self.arena.allocator();
    var args: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer args.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        return try args.toOwnedSlice();
    }

    self.nextToken();

    const exp = try self.parseExpression(.lower);
    try args.append(exp);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const exp2 = try self.parseExpression(.lower);
        try args.append(exp2);
    }

    if (!self.expectPeek(.@")")) return error.MissingRightParenteses;

    const args_owned = try args.toOwnedSlice();

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
    var elements: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer elements.deinit();

    self.nextToken();
    const element1 = try self.parseExpression(.lower);
    try elements.append(element1);

    while (self.expectPeek(.@",")) {
        self.nextToken(); // next_element

        const element_n = try self.parseExpression(.lower);
        try elements.append(element_n);
    }

    if (!self.expectPeek(.@"]")) return error.UnexpectedToken;

    return .{
        .array_literal = .{ .token = token, .elements = try elements.toOwnedSlice() },
    };
}

// var hash = {1:1, 2:2, 3:3}
pub fn parseHashLiteral(self: *Parser) anyerror!ast.Expression {
    const token = self.cur_token;
    const allocator = self.arena.allocator();

    var hash: std.AutoHashMap(*ast.Expression, *ast.Expression) = .init(allocator);
    errdefer hash.deinit();

    while (!self.peekTokenIs(.@"}")) {
        self.nextToken();

        const key = try self.parseExpression(.lower);

        if (!self.expectPeek(.@":")) {
            if (self.expectPeek(.@",") or self.expectPeek(.@"}")) {
                try hash.put(key, key);
                if (self.curTokenIs(.@"}")) break;
                continue;
            }

            std.log.warn("syntax error: expect ':'\n", .{});
            return error.MissingValueInHash;
        }

        self.nextToken();

        const val = try self.parseExpression(.lower);

        // BUG: memory leak
        try hash.put(key, val);

        if (!self.peekTokenIs(.@"}") and !self.expectPeek(.@",")) {
            std.log.warn("syntax error: expect ',' or '}}'\n", .{});
            return error.UnexpectedToken;
        }
    }

    if (!self.expectPeek(.@"}")) {
        std.log.warn("syntax error: expect '}}'\n", .{});
        return error.MissingRightBrace;
    }

    return .{ .hash_literal = .{ .token = token, .pairs = hash } };
}

// (...)
// pub fn parseExpressionList(self: *Parser, exp1: *ast.Expression, end: Token.Type) anyerror![]ast.Expression {
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
    var exp: ast.IndexExpression = .{
        .token = self.cur_token,
        .left = left,
        .index = undefined,
    };

    self.nextToken();

    exp.index = try self.parseExpression(.lower);

    if (!self.expectPeek(.@"]")) return error.MissingRightBracket;

    return .{ .index_expression = exp };
}

// pub fn parseSwitchExpression(self: *Parser) anyerror!ast.Expression {
//     var switch_expression = ast.SwitchExpression{
//         .token = self.cur_token,
//         .value = undefined,
//         .choices = undefined,
//     };
//
//     self.nextToken();
//
//     switch_expression.value = try self.parseExpression(.lower);
//
//     if (!self.expectPeek(.@"{")) return error.MissingBrance;
//
//     self.nextToken();
//
//     const allocator = self.arena.allocator();
//
//     var choices = std.ArrayList(ast.SwitchChoices).init(allocator);
//     errdefer choices.deinit();
//
//     while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {
//         var switch_choices: ast.SwitchChoices = undefined;
//
//         // TODO: deixa o else ser feliz :: ele participa do vetor de exp e entra como
//         // um braco alternativo do switch la no evalSwitch
//         if (self.curTokenIs(.@"else")) {
//             switch_choices = .{
//                 .token = self.cur_token, // .else
//                 .exps = null, // else case
//                 .block = undefined,
//             };
//
//             self.nextToken();
//         } else {
//             if (self.curTokenIs(.@"}")) return error.SyntaxError;
//
//             var exps = std.ArrayList(ast.Expression).init(allocator);
//             errdefer exps.deinit();
//
//             const exp1 = try self.parseExpression(.lower);
//             try exps.append(exp1.*);
//
//             self.nextToken();
//
//             // TODO: parse "..." range
//             while (self.expectPeek(.@",")) {
//                 const exp_n = try self.parseExpression(.lower);
//                 try exps.append(exp_n.*);
//                 self.nextToken();
//             }
//
//             switch_choices = .{
//                 .token = self.cur_token,
//                 .exps = try exps.toOwnedSlice(),
//                 .block = undefined,
//             };
//         }
//
//         if (self.cur_token.type != .@"=>") {
//             return error.UnexpectedToken;
//         }
//
//         self.nextToken();
//
//         if (self.cur_token.type != .@"{") {
//             return error.MissingBlockBrace;
//         }
//
//         switch_choices.block = try self.parseBlockStatement();
//
//         self.nextToken(); // }
//
//         try choices.append(switch_choices);
//     }
//
//     const choice_owned = try choices.toOwnedSlice();
//
//     switch_expression.choices = choice_owned;
//
//     return .{ .switch_expression = switch_expression };
// }
//
// pub fn parseRangeExpression(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
//     var range = ast.RangeExpression{
//         .token = self.cur_token,
//         .start = left,
//         .end = undefined,
//     };
//
//     self.nextToken();
//
//     range.end = try self.parseExpression(.lower);
//
//     return .{ .range = range };
// }
//
// pub fn parseMultiForLoopRange(self: *Parser, flr: ast.ForLoopRangeExpression) anyerror!ast.Expression {
//     const allocator = self.arena.allocator();
//
//     var mflr = ast.MultiForLoopRangeExpression{
//         .token = flr.token,
//         .body = undefined,
//         .loops = undefined,
//     };
//
//     var loops = try std.ArrayList(ast.MultiForLoopRangeExpression.LoopVars).initCapacity(allocator, 1);
//     errdefer loops.deinit();
//
//     try loops.append(.{ .index = flr.index, .ident = flr.ident, .iterable = flr.iterable });
//
//     self.nextToken();
//     while (!self.curTokenIs(.@"{")) {
//         const ident = try self.parseIdentifier();
//
//         var loop_elements = ast.MultiForLoopRangeExpression.LoopVars{
//             .ident = ident.identifier.value,
//             .iterable = undefined,
//         };
//
//         if (self.expectPeek(.@",")) {
//             self.nextToken();
//             const index = try self.parseIdentifier();
//             loop_elements.index = index.identifier.value;
//         }
//
//         if (!self.expectPeek(.in)) {
//             return error.ExpectTheInIdentifier;
//         }
//
//         self.nextToken();
//
//         loop_elements.iterable = try self.parseExpression(.lower);
//
//         self.nextToken();
//
//         try loops.append(loop_elements);
//
//         if (self.curTokenIs(.@";")) {
//             self.nextToken();
//             if (self.curTokenIs(.@"{")) break;
//         }
//     }
//
//     mflr.loops = try loops.toOwnedSlice();
//
//     if (!self.curTokenIs(.@"{")) return error.MissingBrance1;
//
//     mflr.body = try self.parseBlockStatement();
//
//     return .{ .multi_forloop_range_expression = mflr };
// }
//
// pub fn parseForLoopRange(self: *Parser, tk: Token, ident: *ast.Expression) anyerror!ast.Expression {
//     var flr = ast.ForLoopRangeExpression{
//         .token = tk,
//         .ident = ident.identifier.value, // for <ident>[, <idx>] in <range> {
//         .body = undefined,
//         .iterable = undefined,
//     };
//
//     if (self.expectPeek(.@",")) {
//         self.nextToken();
//         const index = try self.parseIdentifier();
//         flr.index = index.identifier.value;
//     }
//
//     if (!self.expectPeek(.in)) {
//         return error.ExpectTheInIdentifier;
//     }
//
//     self.nextToken();
//
//     flr.iterable = try self.parseExpression(.lower);
//
//     if (self.expectPeek(.@";") and !self.peekTokenIs(.@"{")) {
//         return self.parseMultiForLoopRange(flr);
//     }
//
//     if (!self.expectPeek(.@"{")) return error.MissingBrance1;
//
//     flr.body = try self.parseBlockStatement();
//
//     return .{ .forloop_range_expression = flr };
// }
//
// pub fn parseForLoopCondition(self: *Parser, tk: Token, cond: *ast.Expression) anyerror!ast.Expression {
//     var fl = ast.ForLoopExpression{
//         .token = tk,
//         .condition = cond,
//         .consequence = undefined,
//     };
//
//     if (!self.expectPeek(.@"{")) return error.MissingBrance;
//
//     fl.consequence = try self.parseBlockStatement();
//
//     return .{ .forloop_expression = fl };
// }
//
// /// for true { } or for idx, val in list {}
// pub fn parseForLoop(self: *Parser) anyerror!ast.Expression {
//     const token = self.cur_token;
//
//     if (self.expectPeek(.@"{")) {
//         return error.ExpectSomeConditionOrIdentifier;
//     }
//
//     self.nextToken();
//
//     const condition_or_ident = try self.parseExpression(.lower);
//
//     if (condition_or_ident.* == .identifier) {
//         return self.parseForLoopRange(token, condition_or_ident);
//     }
//
//     return self.parseForLoopCondition(token, condition_or_ident);
// }
// fn parseEnumLiteral(self: *Parser) anyerror!ast.Expression {
//     var enu = ast.EnumLiteral{
//         .token = self.cur_token,
//         .tags = undefined, // hash(ident, value)
//     };
//
//     if (!self.expectPeek(.@"{")) return error.MissingBrance;
//
//     const allocator = self.arena.allocator();
//
//     var tags = std.StringHashMap(*ast.Expression).init(allocator);
//     errdefer tags.deinit();
//
//     var i: usize = 0;
//     while (!self.expectPeek(.@"}")) {
//         const ident_exp = try self.parseIdentifier();
//
//         const gop = try tags.getOrPut(ident_exp.identifier.value);
//
//         if (gop.found_existing) return error.DuplicatedTag;
//
//         const int = try allocator.create(ast.Expression);
//
//         int.* = .{
//             .integer_literal = .{
//                 .value = @intCast(i),
//                 .token = .{ .type = .int, .literal = try std.fmt.allocPrint(allocator, "{d}", .{i}) },
//             },
//         };
//
//         try tags.put(ident_exp.identifier.value, int);
//
//         i += 1;
//
//         self.nextToken();
//     }
//
//     enu.tags = tags;
//
//     return .{ .enum_literal = enu };
// }

const PrefixParseFn = *const fn (*Parser) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, *ast.Expression) anyerror!ast.Expression;

const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
