const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
const Parser = @This();

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
arena: std.heap.ArenaAllocator,
errors: Error,

const Error = struct {
    msg: std.ArrayList(u8),

    fn append(err: *Error, comptime fmt: []const u8, args: anytype) !void {
        try err.msg.writer().print(fmt, args);
    }
};

fn missing(p: *Parser, tk: Token.Type) !void {
    try p.errors.append(
        "{}:{} Syntax error: Expect '{s}', found '{s}'\n",
        .{ p.lexer.line_index, p.lexer.position, @tagName(tk), p.peek_token.literal },
    );
}

fn errlog(p: *Parser, msg: []const u8) !void {
    try p.errors.append(
        "{}:{} {s}\n",
        .{ p.lexer.line_index, p.lexer.position, msg },
    );
}

var NULL: ast.Expression = .null;
var TRUE: ast.Expression = .{ .boolean = .{ .value = true } };

pub const Precedence = enum {
    lowest,
    assigne,
    lor,
    land,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,
    index,

    pub fn peek(token_type: Token.Type) Precedence {
        return switch (token_type) {
            else => .lowest,
            .@"or" => .lor,
            .@"and" => .land,
            .@"==", .@"!=" => .equals,
            .@">", .@">=", .@"<", .@"<=" => .lessgreater,
            .@"+", .@"-" => .sum,
            .@"/", .@"*", .@"%" => .product,
            .@"(" => .call,
            .@"!" => .prefix,
            .@"[", .@".", .@"..", .@"..=" => .index,
            .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
        };
    }
};

fn prefixExp(p: *Parser) anyerror!*ast.Expression {
    const allocator = p.arena.allocator();
    const left_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(left_exp);

    left_exp.* = switch (p.cur_token.type) {
        .identifier => p.parseIdentifier(),
        .@"." => p.parseTag(),
        .integer => try p.parseInteger(),
        .float => try p.parseFloat(),
        .true, .false => p.parseBoolean(),
        .null, .@";", .@"}" => p.parseNull(),
        .@"[" => try p.parseArray(),
        .@"{" => try p.parseHash(),
        .new => try p.parseInstance2(),
        .@"fn" => try p.parseFunction(),
        .@"struct", .@"enum" => try p.parseType(),
        .string => p.parseString(),
        .char => p.parseChar(),
        .@"if", .@"else" => try p.parseIf(),
        .@"for" => try p.parseFor(),
        .match => try p.parseMatch(),
        .@"!", .@"-" => try p.parsePrefix(),
        .@"(" => try p.parseGroup(),
        else => b: {
            try p.errlog("Undefined Predix Function");
            std.log.err("At token: {s}", .{p.cur_token.literal});
            break :b NULL;
        },
    };

    return left_exp;
}

fn infixExp(p: *Parser, lx: *ast.Expression) anyerror!?ast.Expression {
    switch (p.peek_token.type) {
        .@"[" => {
            p.nextToken();
            return try p.parseIndex(lx);
        },
        .@"..", .@"..=" => {
            p.nextToken();
            return try p.parseRange(lx);
        },
        .@"." => {
            p.nextToken();
            return try p.parseMethod(lx);
        },
        // .@"{" => {
        //     // if (p.cur_token.type != .identifier) {
        //     //     return null;
        //     // }
        //
        //     p.nextToken();
        //     return try p.parseInstance(lx);
        // },
        .@"(" => {
            p.nextToken();
            return try p.parseCall(lx);
        },
        .@"%", .@"+", .@"-", .@"==", .@"!=", .@"*", .@"or", .@"and", .@"/", .@">", .@">=", .@"<", .@"<=" => {
            p.nextToken();
            return try p.parseInfix(lx);
        },
        .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => {
            p.nextToken();
            return try p.parseAssignment(lx);
        },

        else => return null,
    }
}

pub fn init(child_alloc: std.mem.Allocator, lexer: *Lexer) Parser {
    var arena: std.heap.ArenaAllocator = .init(child_alloc);
    const allocator = arena.allocator();

    var parser: Parser = .{
        .arena = arena,
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .last_token = .{ .type = .eof, .literal = "" },
        .errors = .{ .msg = .init(allocator) },
    };

    parser.nextToken();
    parser.nextToken();

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
fn parseIdentifier(self: *const Parser) ast.Expression {
    return .{ .identifier = .{ .value = self.cur_token.literal } };
}

fn parseTag(self: *Parser) ast.Expression {
    self.nextToken();
    return .{ .tag = .{ .value = self.cur_token.literal } };
}

fn parseReturn(self: *Parser) !ast.Statement {
    var return_stmt: ast.Return = .{ .value = &NULL };

    self.nextToken();

    return_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"return" = return_stmt };
}

fn parseBreak(self: *Parser) !ast.Statement {
    var break_stmt: ast.Break = .{ .value = undefined };

    self.nextToken();

    break_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"break" = break_stmt };
}

fn parseContinue(self: *Parser) !ast.Statement {
    var break_stmt: ast.Continue = .{
        .value = undefined,
    };

    self.nextToken();

    break_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"continue" = break_stmt };
}

fn parseAssignment(self: *Parser, name: *ast.Expression) !ast.Expression {
    var stmt: ast.Assignment = .{
        .name = name,
        .operator = undefined,
        .value = undefined,
    };

    stmt.name = name;

    const op = self.cur_token;

    self.nextToken();

    stmt.operator = switch (op.type) {
        .@"=", .@":=", .@"+=", .@"-=", .@"/=", .@"*=" => op.type,
        else => b: {
            try self.errors.append(
                "{}:{} At assignment: Invalid operator {s}\n",
                .{ self.lexer.line_index, self.lexer.position, op.literal },
            );
            break :b op.type;
        },
    };

    stmt.value = try self.parseExpression(.lowest);

    return .{ .assignment = stmt };
}

pub fn parseDefer(self: *Parser) !ast.Statement {
    var defer_stmt: ast.Defer = .{
        .body = undefined,
    };

    if (!self.expectPeek(.@"{")) {
        try self.errors.append(
            "{}:{} At defer: Expect Identifier, found {s}\n",
            .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
        );
    }

    defer_stmt.body = try self.parseBlock();

    return .{ .@"defer" = defer_stmt };
}

fn parseCon(self: *Parser) !ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .con_block = try self.parseConBlock(tk) };

    var const_stmt: ast.Con = .{
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        try self.errors.append(
            "{}:{} At con: Expect Identifier, found {s}\n",
            .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
        );
    }

    const_stmt.name = .{
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        try self.errors.append(
            "{}:{} At con: Expect assignment operator (=), found {s}\n",
            .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
        );
    }

    self.nextToken();

    // TODO: pointer?
    const_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .con = const_stmt };
}

fn parseVarBlock(self: *Parser, _: Token) !ast.VarBlock {
    var stmt: ast.VarBlock = .{
        .vars_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.Var) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = self.parseIdentifier();

        if (ident != .identifier) try self.errors.append(
            "{}:{} At var block: Expect Identifier, found {s}\n",
            .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
        );

        self.nextToken();

        if (self.expectPeek(.@"=")) try self.errors.append(
            "{}:{} At var block: Expect '=', found {s}\n",
            .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
        );

        self.nextToken();

        const exp = try self.parseExpression(.lowest);

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

fn parseConBlock(self: *Parser, _: Token) !ast.ConBlock {
    var stmt: ast.ConBlock = .{
        .const_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.Con) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.type != .@")") {
        const ident = self.parseIdentifier();

        if (ident != .identifier) try self.missing(.identifier);

        self.nextToken();

        if (self.expectPeek(.@"=")) try self.missing(.@"=");

        self.nextToken();

        const exp = try self.parseExpression(.lowest);

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

fn parseVar(self: *Parser) !ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .var_block = try self.parseVarBlock(tk) };

    var var_stmt: ast.Var = .{
        .name = undefined,
        .value = undefined,
    };

    if (!self.expectPeek(.identifier)) try self.errors.append(
        "{}:{} At 'var' statement: Expect an identifier, found '{s}'\n",
        .{ self.lexer.line_index, self.lexer.position, self.peek_token.literal },
    );

    var_stmt.name = .{
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.@"=")) {
        // const var_type = try self.parseExpression(.lowest);

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
    var_stmt.value = try self.parseExpression(.lowest);

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

        .@"continue" => try self.parseContinue(),

        .@"fn" => try self.parseFn(),

        .@"defer" => try self.parseDefer(),

        .@"{" => .{ .block = try self.parseBlock() },

        else => try self.parseExpStatement(),
    };
}

/// if, for, match, etc...
fn parseExpStatement(self: *Parser) !ast.Statement {
    const exp_stmt: ast.ExpStatement = .{
        .expression = try self.parseExpression(.lowest),
    };

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .exp_statement = exp_stmt };
}

fn parseExpression(self: *Parser, precedence: Precedence) !*ast.Expression {
    const left_exp = try self.prefixExp();

    const allocator = self.arena.allocator();

    while (@intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        const old_left_exp = try allocator.create(ast.Expression);
        errdefer allocator.destroy(old_left_exp);

        old_left_exp.* = left_exp.*;

        left_exp.* = try infixExp(self, old_left_exp) orelse {
            return left_exp;
        };
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

fn parseFloat(self: *Parser) !ast.Expression {
    return .{ .float = .{
        .value = try std.fmt.parseFloat(f32, self.cur_token.literal),
    } };
}

fn parseChar(self: *Parser) ast.Expression {
    var value: u8 = self.cur_token.literal[0];

    if (self.cur_token.literal.len > 1) {
        if (std.mem.eql(u8, self.cur_token.literal, "\\n")) {
            value = '\n';
        } else if (std.mem.eql(u8, self.cur_token.literal, "\\t")) {
            value = '\t';
        } else if (std.mem.eql(u8, self.cur_token.literal, "\\r")) {
            value = '\r';
        }
    }

    return .{ .char = .{ .value = value } };
}

fn parseInteger(self: *Parser) !ast.Expression {
    return .{ .integer = .{
        .value = try std.fmt.parseInt(i32, self.cur_token.literal, 10),
    } };
}

fn parseIntegerFrom(self: *Parser, i: usize) !*ast.Expression {
    const alloc = self.arena.allocator();
    const int = try alloc.create(ast.Expression);
    int.* = .{ .integer = .{ .value = @intCast(i) } };
    return int;
}

fn parseBoolean(self: *Parser) ast.Expression {
    return .{ .boolean = .{ .value = self.curTokenIs(.true) } };
}

fn parseNull(_: *Parser) ast.Expression {
    return .null;
}

//  BUG
fn parseGroup(self: *Parser) !ast.Expression {
    self.nextToken();

    const exp = try self.parseExpression(.lowest);

    if (!self.expectPeek(.@")")) try self.missing(.@")");

    return exp.*;
}

fn parsePrefix(self: *Parser) !ast.Expression {
    var expression: ast.Prefix = .{
        .operator = self.cur_token.type,
        .right = undefined,
    };

    expression.operator = self.cur_token.type;

    self.nextToken();

    expression.right = try self.parseExpression(.prefix);

    return .{ .prefix = expression };
}

fn parseInfix(self: *Parser, left: *ast.Expression) !ast.Expression {
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

fn parseIf(self: *Parser) !ast.Expression {
    var expression: ast.If = .{
        .condition = undefined,
        .consequence = undefined,
    };

    self.nextToken();

    expression.condition = try self.parseExpression(.lowest);

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    expression.consequence = try self.parseBlock();

    if (self.peekTokenIs(.@"else")) {
        self.nextToken();
        if (!self.expectPeek(.@"{")) try self.missing(.@"{");
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

fn parseFn(self: *Parser) !ast.Statement {
    var func_stmt: ast.FunctionStatement = .{
        .func = undefined,
        .name = undefined,
    };

    if (!self.expectPeek(.identifier)) {
        try self.missing(.identifier);
    }

    func_stmt.name = .{
        .value = self.cur_token.literal,
    };

    func_stmt.func = (try self.parseFunction()).function;

    return .{ .@"fn" = func_stmt };
}

fn parseType(self: *Parser) !ast.Expression {
    var struc: ast.Type = .{ .type = self.cur_token.type };

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    var fields: std.ArrayList(ast.Type.Field) = .init(self.arena.allocator());
    errdefer fields.deinit();

    var descs: std.ArrayList(ast.FunctionStatement) = .init(self.arena.allocator());
    errdefer descs.deinit();

    while (!self.peekTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        self.nextToken();

        if (self.curTokenIs(.identifier)) {
            var field: ast.Type.Field = .{
                .name = self.parseIdentifier().identifier,
                .value = &NULL,
            };

            if (self.expectPeek(.@"=")) {
                self.nextToken();
                field.value = try self.parseExpression(.lowest);
            }

            if (self.peekTokenIs(.@",") or self.curTokenIs(.@";")) {
                self.nextToken();
            }

            try fields.append(field);
            continue;
        }

        if (self.curTokenIs(.@"fn")) {
            if (!self.expectPeek(.identifier)) {
                try self.missing(.identifier);
            }

            const name = self.parseIdentifier().identifier;

            const func = (try self.parseFunction()).function;

            try descs.append(.{
                .name = name,
                .func = func,
            });
            continue;
        }

        try self.errlog("Invalid Struct");
    }

    self.nextToken();

    if (!self.curTokenIs(.@"}")) try self.missing(.@"}");

    struc.fields = try fields.toOwnedSlice();
    struc.desc = try descs.toOwnedSlice();

    return .{ .type = struc };
}

fn parseFunction(self: *Parser) !ast.Expression {
    var func: ast.Function = .{
        .parameters = undefined,
        .body = undefined,
    };

    if (!self.expectPeek(.@"(")) try self.missing(.@"(");

    func.parameters = try self.parseFunctionParameters();

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    func.body = try self.parseBlock();

    return .{ .function = func };
}

fn parseFunctionParameters(self: *Parser) ![]ast.Identifier {
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

    if (!self.expectPeek(.@")")) try self.missing(.@")");

    const ident_owner = try indentifiers.toOwnedSlice();

    return ident_owner;
}

fn parseMethod(self: *Parser, caller: *ast.Expression) !ast.Expression {
    var method_exp: ast.Method = .{
        .caller = caller,
        .method = undefined,
    };

    self.nextToken();

    const exp_mathod = self.parseIdentifier();

    if (exp_mathod != .identifier) try self.missing(.identifier);

    method_exp.method = exp_mathod.identifier;

    return .{ .method = method_exp };
}

fn parseInstance(self: *Parser, type_exp: *ast.Expression) !ast.Expression {
    return .{ .instance = .{ .type = type_exp, .fields = try self.parseInstanceArguments() } };
}

fn parseInstance2(self: *Parser) !ast.Expression {
    self.nextToken();
    return .{ .instance = .{
        .type = try self.parseExpression(.lowest),
        .fields = try self.parseInstanceArguments(),
    } };
}

fn parseInstanceArguments(self: *Parser) ![]ast.Type.Field {
    var fields: std.ArrayList(ast.Type.Field) = .init(self.arena.allocator());
    errdefer fields.deinit();

    if (!self.peekTokenIs(.@"{")) {
        try self.missing(.@"{");
    }

    self.nextToken();

    if (self.peekTokenIs(.@"}")) {
        self.nextToken();
        return try fields.toOwnedSlice();
    }

    while (!self.peekTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        self.nextToken();

        if (self.curTokenIs(.identifier)) {
            var field: ast.Type.Field = .{
                .name = self.parseIdentifier().identifier,
                .value = &NULL,
            };

            if (self.expectPeek(.@":")) {
                self.nextToken();
                field.value = try self.parseExpression(.lowest);
            }

            try fields.append(field);

            if (!self.expectPeek(.@",")) {
                if (self.peekTokenIs(.@"}")) break;
                try self.missing(.@",");
            }

            continue;
        }

        try self.errlog("Syntax error: Invalid Initialization");
    }

    self.nextToken();

    if (!self.curTokenIs(.@"}")) try self.missing(.@"}");

    return fields.toOwnedSlice();
}

fn parseCall(self: *Parser, func: *ast.Expression) !ast.Expression {
    return .{ .call = .{ .function = func, .arguments = try self.parseCallArguments() } };
}

fn parseCallArguments(self: *Parser) ![]*ast.Expression {
    const allocator = self.arena.allocator();
    var args: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer args.deinit();

    if (self.peekTokenIs(.@")")) {
        self.nextToken();
        return try args.toOwnedSlice();
    }

    self.nextToken();

    const exp = try self.parseExpression(.lowest);
    try args.append(exp);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const exp2 = try self.parseExpression(.lowest);
        try args.append(exp2);
    }

    if (!self.expectPeek(.@")")) try self.missing(.@")");

    const args_owned = try args.toOwnedSlice();

    return args_owned;
}

pub fn parseString(self: *Parser) ast.Expression {
    return .{
        .string = .{
            .value = self.cur_token.literal,
        },
    };
}

pub fn parseArray(self: *Parser) !ast.Expression {
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
    const element1 = try self.parseExpression(.lowest);
    try elements.append(element1);

    while (self.expectPeek(.@",")) {
        self.nextToken(); // next_element

        const element_n = try self.parseExpression(.lowest);
        try elements.append(element_n);
    }

    if (!self.expectPeek(.@"]")) try self.missing(.@"]");
    return .{
        .array = .{ .elements = try elements.toOwnedSlice() },
    };
}

// var hash = {1:1, 2:2, 3:3}
pub fn parseHash(self: *Parser) !ast.Expression {
    // const token = self.cur_token;
    const allocator = self.arena.allocator();

    var pairs: std.ArrayList([2]*ast.Expression) = .init(allocator);
    errdefer pairs.deinit();

    var counter: usize = 0;
    while (!self.peekTokenIs(.@"}")) {
        self.nextToken();
        const key = try self.parseExpression(.lowest);

        if (!self.expectPeek(.@":")) {
            if (self.expectPeek(.@",") or self.expectPeek(.@"}")) {
                const index = try self.parseIntegerFrom(counter);
                counter += 1;
                // try pairs.put(index, key);
                try pairs.append(.{ index, key });

                if (self.curTokenIs(.@"}")) {
                    return .{ .hash = .{ .pairs = try pairs.toOwnedSlice() } };
                }

                continue;
            }

            try self.missing(.@":");
        }

        self.nextToken();
        const val = try self.parseExpression(.lowest);

        try pairs.append(.{ key, val });

        if (!self.peekTokenIs(.@"}") and !self.expectPeek(.@",")) {
            try self.missing(.@",");
            try self.missing(.@"}");
        }
    }

    if (!self.expectPeek(.@"}")) try self.missing(.@"}");

    return .{ .hash = .{ .pairs = try pairs.toOwnedSlice() } };
}

// (...)
// pub fn parseExpressionList(self: *Parser, exp1: *ast.Expression, end: Token.Type) ![]ast.Expression {
//     const allocator = self.arena.allocator();
//     var list = std.ArrayList(ast.Expression).init(allocator);
//     errdefer list.deinit();
//
//     try list.append(exp1.*);
//
//     var exp = try self.parseExpression(Precedence.lowest);
//     try list.append(exp.*);
//
//     while (self.peekTokenIs(.@",")) {
//         self.nextToken();
//         self.nextToken();
//         var exp2 = try self.parseExpression(Precedence.lowest);
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
pub fn parseIndex(self: *Parser, left: *ast.Expression) !ast.Expression {
    var exp: ast.Index = .{
        .left = left,
        .index = undefined,
    };

    self.nextToken();

    exp.index = try self.parseExpression(.lowest);

    if (!self.expectPeek(.@"]")) try self.missing(.@"]");

    return .{ .index = exp };
}

pub fn parseMatch(self: *Parser) !ast.Expression {
    var match: ast.Match = .{
        .value = undefined,
        .arms = undefined,
    };

    self.nextToken();

    if (self.curTokenIs(.@"{")) {
        match.value = &TRUE;
    } else {
        match.value = try self.parseExpression(.lowest);
        if (!self.expectPeek(.@"{")) try self.missing(.@"{");
    }

    self.nextToken();

    const allocator = self.arena.allocator();

    var arms = std.ArrayList(ast.Match.Arm).init(allocator);
    errdefer arms.deinit();

    while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {

        // the else arm
        if (self.curTokenIs(.@"else")) {
            if (match.else_block != null) {
                try self.errlog("Duplicated Tag");
            }

            if (!self.expectPeek(.@"=>")) {
                try self.missing(.@"=>");
            }

            if (!self.expectPeek(.@"{")) {
                try self.missing(.@"{");
            }

            match.else_block = try self.parseBlock();

            self.nextToken();

            continue;
        }

        var arm: ast.Match.Arm = .{
            .condition = try self.parseExpression(.lowest),
            .block = undefined,
        };

        if (!self.expectPeek(.@"=>")) {
            try self.missing(.@"=>");
        }

        if (!self.expectPeek(.@"{")) {
            try self.missing(.@"{");
        }

        arm.block = try self.parseBlock();

        self.nextToken(); // jump the }

        try arms.append(arm);
    }

    match.arms = try arms.toOwnedSlice();

    // std.debug.print("{}\n", .{match.value});
    // std.debug.print("{any}\n", .{match.arms});
    // std.debug.print("{?}\n", .{match.else_block});

    return .{ .match = match };
}

pub fn parseRange(self: *Parser, left: *ast.Expression) !ast.Expression {
    var range = ast.Range{
        .start = left,
        .end = undefined,
        .inc = if (self.curTokenIs(.@"..")) .no else .yes,
    };

    self.nextToken();

    range.end = try self.parseExpression(.lowest);

    return .{ .range = range };
}
//
// pub fn parseMultiForLoopRange(self: *Parser, flr: ast.ForLoopRange) !ast.Expression {
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
//         loop_elements.iterable = try self.parseExpression(.lowest);
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
pub fn parseForRange(self: *Parser, ident: ast.Identifier) !ast.Expression {
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
        try self.missing(.in);
    }

    self.nextToken();

    flr.iterable = try self.parseExpression(.lowest);

    // if (self.expectPeek(.@";") and !self.peekTokenIs(.@"{")) {
    //     return self.parseMultiForLoopRange(flr);
    // }

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    flr.body = try self.parseBlock();

    return .{ .for_range = flr };
}

pub fn parseForLoopCondition(self: *Parser, cond: *ast.Expression) !ast.Expression {
    var fl = ast.For{
        .condition = cond,
        .consequence = undefined,
    };

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    fl.consequence = try self.parseBlock();

    return .{ .@"for" = fl };
}

// /// for true { } or for idx, val in list {}
pub fn parseFor(self: *Parser) !ast.Expression {
    if (self.expectPeek(.@"{")) {
        return .{ .@"for" = .{ .condition = &TRUE, .consequence = try self.parseBlock() } };
    }

    self.nextToken();

    const condition_or_ident = try self.parseExpression(.lowest);

    if (condition_or_ident.* == .identifier) {
        if (self.peekTokenIs(.in)) {
            return self.parseForRange(condition_or_ident.identifier);
        }
    }

    return self.parseForLoopCondition(condition_or_ident);
}

// fn parseEnum(self: *Parser) !ast.Expression {
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
