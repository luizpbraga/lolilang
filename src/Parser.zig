const Parser = @This();

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
arena: std.heap.ArenaAllocator,
infix_fns: std.EnumMap(Token.Type, InfixParseFn),
prefix_fns: std.EnumMap(Token.Type, PrefixParseFn),

var NULL: ast.Expression = .null;

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

    const precedences: std.EnumMap(Token.Type, Precedence) = .initFullWithDefault(.lower, .{
        .@"or" = .cond,
        .@"and" = .cond,
        .@"==" = .equals,
        .@"!=" = .equals,
        .@">" = .lessgreater,
        .@">=" = .lessgreater,
        .@"<" = .lessgreater,
        .@"<=" = .lessgreater,
        .@"+" = .sum,
        .@"-" = .sum,
        .@"/" = .product,
        .@"*" = .product,
        .@"(" = .call,
        .@"!" = .prefix,
        .@"[" = .index,
        .@"." = .index,
        .@".." = .index,
        .@"=" = .assigne,
        .@":=" = .assigne,
        .@"+=" = .assigne,
        .@"-=" = .assigne,
        .@"*=" = .assigne,
        .@"/=" = .assigne,
    });

    pub fn peek(token_type: Token.Type) Precedence {
        return precedences.get(token_type).?;
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
    parser.prefix_fns.put(.@".", parseEnumTag);
    parser.prefix_fns.put(.integer, parseInteger);
    parser.prefix_fns.put(.float, parseFloat);
    parser.prefix_fns.put(.true, parseBoolean);
    parser.prefix_fns.put(.false, parseBoolean);
    parser.prefix_fns.put(.null, parseNull);
    parser.prefix_fns.put(.@";", parseNull);
    parser.prefix_fns.put(.@"[", parseArray);
    parser.prefix_fns.put(.@"{", parseHash);
    parser.prefix_fns.put(.@"fn", parseFunction);
    parser.prefix_fns.put(.string, parseString);
    parser.prefix_fns.put(.char, parseChar);
    parser.prefix_fns.put(.@"if", parseIf);
    parser.prefix_fns.put(.@"for", parseFor);
    // parser.prefix_fns.put(.@"switch", parseSwitch);
    // parser.prefix_fns.put(.@"enum", parseEnum);
    parser.prefix_fns.put(.@"else", parseIf);
    parser.prefix_fns.put(.@"!", parsePrefix);
    parser.prefix_fns.put(.@"-", parsePrefix);
    parser.prefix_fns.put(.@"(", parseGroup);

    parser.infix_fns.put(.@"[", parseIndex);
    parser.infix_fns.put(.@"..", parseRange);
    parser.infix_fns.put(.@".", parseMethod);
    parser.infix_fns.put(.@"(", parseCall);
    parser.infix_fns.put(.@"+", parseInfix);
    parser.infix_fns.put(.@"-", parseInfix);
    parser.infix_fns.put(.@":", parseInfix);
    parser.infix_fns.put(.@"==", parseInfix);
    parser.infix_fns.put(.@"!=", parseInfix);
    parser.infix_fns.put(.@"*", parseInfix);
    parser.infix_fns.put(.@"or", parseInfix);
    parser.infix_fns.put(.@"and", parseInfix);
    parser.infix_fns.put(.@"/", parseInfix);
    parser.infix_fns.put(.@">", parseInfix);
    parser.infix_fns.put(.@">=", parseInfix);
    parser.infix_fns.put(.@"<", parseInfix);
    parser.infix_fns.put(.@"<=", parseInfix);
    parser.infix_fns.put(.@"=", parseAssignment);
    parser.infix_fns.put(.@":=", parseAssignment);
    parser.infix_fns.put(.@"+=", parseAssignment);
    parser.infix_fns.put(.@"-=", parseAssignment);
    parser.infix_fns.put(.@"*=", parseAssignment);
    parser.infix_fns.put(.@"/=", parseAssignment);

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
            .value = self.cur_token.literal,
        },
    };
}

fn parseEnumTag(self: *Parser) anyerror!ast.Expression {
    self.nextToken();
    return .{
        .enum_tag = .{
            .value = self.cur_token.literal,
        },
    };
}

fn parseReturn(self: *Parser) anyerror!ast.Statement {
    var return_stmt: ast.Return = .{
        .value = &NULL,
    };

    self.nextToken();

    return_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"return" = return_stmt };
}

fn parseBreak(self: *Parser) anyerror!ast.Statement {
    var break_stmt: ast.Break = .{
        .value = undefined,
    };

    self.nextToken();

    break_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"break" = break_stmt };
}

fn parseAssignment(self: *Parser, name: *ast.Expression) anyerror!ast.Expression {
    var stmt: ast.Assignment = .{
        .name = name,
        .operator = undefined,
        .value = undefined,
    };

    const op = self.cur_token;

    self.nextToken();

    stmt.operator = switch (op.type) {
        .@"=", .@":=", .@"+=", .@"-=", .@"/=", .@"*=" => op.type,
        else => return error.UnexpectedToken,
    };

    stmt.value = try self.parseExpression(.lower);

    return .{ .assignment = stmt };
}

pub fn parseDefer(self: *Parser) anyerror!ast.Statement {
    var defer_stmt: ast.Defer = .{
        .body = undefined,
    };

    if (!self.expectPeek(.@"{")) {
        return error.ExpectSomeConditionOrIdentifier;
    }

    defer_stmt.body = try self.parseBlock();

    return .{ .@"defer" = defer_stmt };
}

fn parseCon(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .con_block = try self.parseConBlock(tk) };

    var const_stmt: ast.Con = .{
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
        return error.UnexpectedToken;
    }

    const_stmt.name = .{
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

    return .{ .con = const_stmt };
}

fn parseVarBlock(self: *Parser, _: Token) anyerror!ast.VarBlock {
    var stmt: ast.VarBlock = .{
        .vars_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.Var) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        const var_stmt = ast.Var{
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

fn parseConBlock(self: *Parser, _: Token) anyerror!ast.ConBlock {
    var stmt: ast.ConBlock = .{
        .const_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.Con) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = try self.parseIdentifier();

        if (ident != .identifier) return error.ExprectIdentifier;

        self.nextToken();

        if (self.expectPeek(.@"=")) return error.MissingAssingmentOperator;

        self.nextToken();

        const exp = try self.parseExpression(.lower);

        const var_stmt: ast.Con = .{
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

fn line(self: *Parser) []const u8 {
    const input = self.lexer.input;
    return input[self.lexer.position..];
}

fn parseVar(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .var_block = try self.parseVarBlock(tk) };

    var var_stmt: ast.Var = .{
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}' at {s}", .{ self.cur_token.literal, self.line() });
        return error.UnexpectedToken;
    }

    var_stmt.name = .{
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        // const var_type = try self.parseExpression(.lower);

        self.nextToken();

        if (!self.expectPeek(.@"=")) {
            var_stmt.value = &NULL;

            if (self.peekTokenIs(.@";")) {
                self.nextToken();
            }

            return .{ .@"var" = var_stmt };
        }
    }

    self.nextToken();

    // TODO: pointer?
    var_stmt.value = try self.parseExpression(.lower);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"var" = var_stmt };
}

fn parseStatement(self: *Parser) !ast.Statement {
    return switch (self.cur_token.type) {
        .@"var" => try self.parseVar(),

        .con => try self.parseCon(),

        .@"return" => try self.parseReturn(),

        .@"break" => try self.parseBreak(),

        .@"fn" => try self.parseFn(),

        .@"defer" => try self.parseDefer(),

        .@"{" => .{ .block = try self.parseBlock() },

        else => try self.parseExpStatement(),
    };
}

fn parseExpStatement(self: *Parser) anyerror!ast.Statement {
    const exp_stmt: ast.ExpStatement = .{
        .expression = try self.parseExpression(.lower),
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .exp_statement = exp_stmt };
}

// fn parseType(self: *Parser) anyerror!*ast.Expression {
//     const value = try self.parseExpression(.lower);
//     return .{ .type = .{ .type = value.identifier } };
// }

fn parseExpression(self: *Parser, precedence: Precedence) !*ast.Expression {
    const prefixFn = self.prefix_fns.get(self.cur_token.type) orelse {
        std.log.warn("At token {s}, {s}\n", .{ self.cur_token.literal, self.line() });
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

    var stmt: ast.Statement = undefined;
    while (self.cur_token.type != .eof) {
        stmt = try self.parseStatement();
        try stmts.append(stmt);
        self.nextToken();
    }

    return .{ .statements = stmts };
}

pub fn parse(self: *Parser) !ast.Node {
    const program = try self.parseProgram();
    return .{ .statement = .{ .program = program } };
}

fn parseFloat(self: *Parser) anyerror!ast.Expression {
    return .{
        .float = .{
            .value = std.fmt.parseFloat(f32, self.cur_token.literal) catch |err| {
                std.log.err("Could not parse {s} as float", .{self.cur_token.literal});
                return err;
            },
        },
    };
}

fn parseChar(self: *Parser) anyerror!ast.Expression {
    return .{ .char = .{ .value = self.cur_token.literal[0] } };
}

fn parseInteger(self: *Parser) anyerror!ast.Expression {
    return .{
        .integer = .{
            .value = std.fmt.parseInt(i32, self.cur_token.literal, 10) catch |err| {
                std.log.err("Could not parse {s} as integer", .{self.cur_token.literal});
                return err;
            },
        },
    };
}

fn parseBoolean(self: *Parser) anyerror!ast.Expression {
    return .{ .boolean = .{ .value = self.curTokenIs(.true) } };
}

fn parseNull(_: *Parser) anyerror!ast.Expression {
    return .null;
}

//  BUG
fn parseGroup(self: *Parser) anyerror!ast.Expression {
    self.nextToken();

    const exp = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@")")) return error.MissingParentese;

    return exp.*;
}

fn parsePrefix(self: *Parser) anyerror!ast.Expression {
    var expression: ast.Prefix = .{
        .operator = self.cur_token.type,
        .right = undefined,
    };

    self.nextToken();

    expression.right = try self.parseExpression(.prefix);

    return .{ .prefix = expression };
}

fn parseInfix(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    var infix = ast.Infix{
        .operator = self.cur_token.type,
        .left = left,
        .right = undefined,
    };

    const precedence = self.currentPrecedence();

    self.nextToken();

    infix.right = try self.parseExpression(precedence);

    return .{ .infix = infix };
}

fn parseIf(self: *Parser) anyerror!ast.Expression {
    var expression: ast.If = .{
        .condition = undefined,
        .consequence = undefined,
    };

    self.nextToken();

    expression.condition = try self.parseExpression(Precedence.lower);

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    expression.consequence = try self.parseBlock();

    if (self.peekTokenIs(.@"else")) {
        self.nextToken();
        if (!self.expectPeek(.@"{")) return error.MissingBrance;
        expression.alternative = try self.parseBlock();
    }

    return .{ .@"if" = expression };
}

fn parseBlock(self: *Parser) anyerror!ast.Block {
    const allocator = self.arena.allocator();

    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();

    var block: ast.Block = .{
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

fn parseFn(self: *Parser) anyerror!ast.Statement {
    var func_stmt: ast.FunctionStatement = .{
        .func = undefined,
        .name = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        std.log.err("Expect Identifier, found '{s}'", .{self.cur_token.literal});
        return error.UnexpectedToken;
    }

    func_stmt.name = .{
        .value = self.cur_token.literal,
    };

    func_stmt.func = (try self.parseFunction()).function;

    return .{ .@"fn" = func_stmt };
}

fn parseFunction(self: *Parser) anyerror!ast.Expression {
    var func: ast.Function = .{
        .parameters = undefined,
        .body = undefined,
    };

    if (!self.expectPeek(.@"(")) return error.MissingParentese;

    func.parameters = try self.parseFunctionParameters();

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    func.body = try self.parseBlock();

    return .{ .function = func };
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
        .value = self.cur_token.literal,
    };

    try indentifiers.append(ident);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const ident2: ast.Identifier = .{
            .value = self.cur_token.literal,
        };

        try indentifiers.append(ident2);
    }

    if (!self.expectPeek(.@")"))
        return error.MissingParentese;

    const ident_owner = try indentifiers.toOwnedSlice();

    return ident_owner;
}

fn parseMethod(self: *Parser, caller: *ast.Expression) anyerror!ast.Expression {
    var method_exp: ast.Method = .{
        .caller = caller,
        .method = undefined,
    };

    self.nextToken();

    const exp_mathod = try self.parseIdentifier();

    if (exp_mathod != .identifier) return error.ExpectIdentifier;

    method_exp.method = exp_mathod.identifier;

    return .{ .method = method_exp };
}

fn parseCall(self: *Parser, func: *ast.Expression) anyerror!ast.Expression {
    return .{
        .call = .{
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

pub fn parseString(self: *Parser) anyerror!ast.Expression {
    return .{
        .string = .{
            .value = self.cur_token.literal,
        },
    };
}

pub fn parseArray(self: *Parser) anyerror!ast.Expression {
    // const token = self.cur_token;

    if (self.peekTokenIs(.@"]")) {
        self.nextToken();
        return .{
            .array = .{ .elements = &.{} },
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
        .array = .{ .elements = try elements.toOwnedSlice() },
    };
}

// var hash = {1:1, 2:2, 3:3}
pub fn parseHash(self: *Parser) anyerror!ast.Expression {
    // const token = self.cur_token;
    const allocator = self.arena.allocator();

    var hash: std.AutoHashMap(*ast.Expression, *ast.Expression) = .init(allocator);
    errdefer hash.deinit();

    while (!self.peekTokenIs(.@"}")) {
        self.nextToken();

        const key = try self.parseExpression(.lower);

        if (!self.expectPeek(.@":")) {
            if (self.expectPeek(.@",") or self.expectPeek(.@"}")) {
                try hash.put(key, &NULL);
                if (self.curTokenIs(.@"}")) {
                    return .{ .hash = .{ .pairs = hash } };
                }
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

    return .{ .hash = .{ .pairs = hash } };
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
pub fn parseIndex(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    var exp: ast.Index = .{
        .left = left,
        .index = undefined,
    };

    self.nextToken();

    exp.index = try self.parseExpression(.lower);

    if (!self.expectPeek(.@"]")) return error.MissingRightBracket;

    return .{ .index = exp };
}

// pub fn parseSwitch(self: *Parser) anyerror!ast.Expression {
//     var switch = ast.Switch{
//         .token = self.cur_token,
//         .value = undefined,
//         .choices = undefined,
//     };
//
//     self.nextToken();
//
//     switch.value = try self.parseExpression(.lower);
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
//             var exps = std.ArrayList(ast.).init(allocator);
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
//     switch.choices = choice_owned;
//
//     return .{ .switch = switch };
// }
//
pub fn parseRange(self: *Parser, left: *ast.Expression) anyerror!ast.Expression {
    var range = ast.Range{
        .start = left,
        .end = undefined,
    };

    self.nextToken();

    range.end = try self.parseExpression(.lower);

    return .{ .range = range };
}
//
// pub fn parseMultiForLoopRange(self: *Parser, flr: ast.ForLoopRange) anyerror!ast.Expression {
//     const allocator = self.arena.allocator();
//
//     var mflr = ast.MultiForLoopRange{
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
//         var loop_elements = ast.MultiForLoopRange.LoopVars{
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
//     return .{ .multi_forloop_range = mflr };
// }
//
pub fn parseForRange(self: *Parser, tk: Token, ident: ast.Identifier) anyerror!ast.Expression {
    _ = tk;
    var flr = ast.ForRange{
        .ident = ident.value, // for <ident>[, <idx>] in <range> {
        .body = undefined,
        .iterable = undefined,
    };

    // if (self.expectPeek(.@",")) {
    //     self.nextToken();
    //     const index = try self.parseIdentifier();
    //     flr.index = index.identifier.value;
    // }
    //
    if (!self.expectPeek(.in)) {
        return error.ExpectTheInIdentifier;
    }

    self.nextToken();

    flr.iterable = try self.parseExpression(.lower);

    // if (self.expectPeek(.@";") and !self.peekTokenIs(.@"{")) {
    //     return self.parseMultiForLoopRange(flr);
    // }

    if (!self.expectPeek(.@"{")) return error.MissingBrance1;

    flr.body = try self.parseBlock();

    return .{ .for_range = flr };
}

pub fn parseForLoopCondition(self: *Parser, _: Token, cond: *ast.Expression) anyerror!ast.Expression {
    var fl = ast.For{
        .condition = cond,
        .consequence = undefined,
    };

    if (!self.expectPeek(.@"{")) return error.MissingBrance;

    fl.consequence = try self.parseBlock();

    return .{ .@"for" = fl };
}
//
// /// for true { } or for idx, val in list {}
pub fn parseFor(self: *Parser) anyerror!ast.Expression {
    const token = self.cur_token;

    if (self.expectPeek(.@"{")) {
        return error.ExpectSomeConditionOrIdentifier;
    }

    self.nextToken();

    const condition_or_ident = try self.parseExpression(.lower);

    if (condition_or_ident.* == .identifier) {
        if (self.peekTokenIs(.in)) {
            return self.parseForRange(token, condition_or_ident.identifier);
        }
    }

    return self.parseForLoopCondition(token, condition_or_ident);
}
// fn parseEnum(self: *Parser) anyerror!ast.Expression {
//     var enu = ast.Enum{
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
//             .integer = .{
//                 .value = @intCast(i),
//                 .token = .{ .type = .integer, .literal = try std.fmt.allocPrint(allocator, "{d}", .{i}) },
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
//     return .{ .enum = enu };
// }

const PrefixParseFn = *const fn (*Parser) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, *ast.Expression) anyerror!ast.Expression;

const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
