const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
const Parser = @This();
const File = @import("Line.zig");
const Error = @import("Error.zig");
const Allocator = std.mem.Allocator;

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
call: bool = false,
errors: *Error,

pub fn init(lexer: *Lexer, errors: *Error) Parser {
    var parser: Parser = .{
        .lexer = lexer,
        .errors = errors,
        .cur_token = undefined,
        .peek_token = undefined,
        .last_token = .{ .tag = .eof },
    };
    parser.nextToken();
    parser.nextToken();
    return parser;
}

fn prefixExp(p: *Parser, arena: Allocator) anyerror!*ast.Expression {
    const left_exp = try p.newExp(arena);
    if (!p.call and p.peekTokenIs(.@",")) {
        return p.parseTuple(arena, left_exp);
    }
    return left_exp;
}

fn infixExp(p: *Parser, arena: Allocator, lx: *ast.Expression) anyerror!?ast.Expression {
    var tk: Token = p.peek_token;
    const left_exp: ?ast.Expression = b: switch (tk.tag) {
        .@"[" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseIndex(arena, lx);
        },
        .@"..", .@"..=" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseRange(arena, lx);
        },
        .@"." => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseMethod(arena, lx);
        },
        .@"{" => {
            // p.nextToken(, );
            tk = p.cur_token;
            break :b try p.parseInstance(arena, lx);
        },
        .@"(" => {
            p.nextToken();
            tk = p.cur_token;
            p.call = true;
            defer p.call = false;
            break :b try p.parseCall(arena, lx);
        },

        .@"%", .@"+", .@"-", .@"==", .@"!=", .@"*", .@"or", .@"and", .@"/", .@">", .@">=", .@"^" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseInfix(arena, lx);
        },

        .@"<", .@"<=" => {
            // a < b < c
            // a < b and b < c
            p.nextToken();
            tk = p.cur_token;
            const infix_left = try p.parseInfix(arena, lx);

            // CLASSIC a < b OR a <= b
            if (!p.expectPeek(.@"<") and !p.expectPeek(.@"<=")) {
                break :b infix_left;
            }

            // FANCY a < b < c
            tk = p.cur_token;
            const infix_rigth = try p.parseInfix(arena, infix_left.infix.right);

            const l = try arena.create(ast.Expression);
            const r = try arena.create(ast.Expression);
            l.* = infix_left;
            r.* = infix_rigth;
            break :b .{ .infix = .{
                .operator = .@"and",
                .left = l,
                .right = r,
            } };
        },
        .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseAssignment(arena, lx);
        },

        else => break :b null,
    };

    return left_exp;
}

fn nextToken(p: *Parser) void {
    p.last_token = p.cur_token;
    p.cur_token = p.peek_token;
    p.peek_token = p.lexer.nextToken();
}

fn curTokenIs(p: *const Parser, token_type: Token.Tag) bool {
    return p.cur_token.tag == token_type;
}

fn peekTokenIs(p: *const Parser, token_type: Token.Tag) bool {
    return p.peek_token.tag == token_type;
}

/// match and eat the peeked token (if true)
fn expectPeek(p: *Parser, token_type: Token.Tag) bool {
    if (p.peekTokenIs(token_type)) {
        p.nextToken();
        return true;
    }
    return false;
}

fn peekPrecedence(p: *const Parser) Precedence {
    return .peek(p.peek_token.tag);
}

fn currentPrecedence(p: *const Parser) Precedence {
    return .peek(p.cur_token.tag);
}

// parsers fn -----------------------------------------------------------
fn parseIdentifier(p: *const Parser) ast.Expression {
    return .{ .identifier = .{
        .value = p.tokenliteral(),
    } };
}

fn parseTag(p: *Parser) ast.Expression {
    p.nextToken();
    return .{ .tag = .{
        .value = p.tokenliteral(),
    } };
}

fn parseReturn(p: *Parser, arena: Allocator) !ast.Statement {
    var return_stmt: ast.Return = .{ .value = &NULL, .token = p.cur_token };

    if (p.peekTokenIs(.@"}")) {
        return .{ .@"return" = return_stmt };
    }

    p.nextToken();

    var value = try p.parseExpression(arena, .lowest);
    if (p.peekTokenIs(.@",")) {
        value = try p.parseTuple(arena, value);
    }

    if (p.peekTokenIs(.@";")) {
        p.nextToken();
    }

    return_stmt.value = value;

    return .{ .@"return" = return_stmt };
}

fn parsePub(p: *Parser, arena: Allocator) anyerror!ast.Statement {
    const tk = p.cur_token;
    p.nextToken();

    const func_or_var = try p.parseStatement(arena);

    switch (func_or_var) {
        .@"fn", .@"var", .import => {},
        else => try p.errlog(arena, "Invalid public declaration"),
    }

    const stmt = try arena.create(ast.Statement);
    stmt.* = func_or_var;

    return .{ .@"pub" = .{ .token = tk, .stmt = stmt } };
}

fn findImportName(p: *Parser, arena: Allocator, path: []const u8) ![]const u8 {
    if (!std.mem.endsWith(u8, path, ".loli")) {
        try p.errlog(arena, "Invalid Module Path: expected .loli extention, got your mama");
    }
    var start = std.mem.lastIndexOf(u8, path, "/") orelse 0;
    if (start != 0) start += 1;
    const end = std.mem.lastIndexOf(u8, path, ".") orelse return error.InvalidPath;
    return path[start..end];
}

// import "fmt"
fn parseImport(p: *Parser, arena: Allocator) anyerror!ast.Statement {
    const tk = p.cur_token;
    p.nextToken();

    var name: []const u8 = "";
    if (p.curTokenIs(.identifier)) {
        name = p.tokenliteral();
        p.nextToken();
    }

    const path_exp = try p.parseExpression(arena, .lowest);

    if (path_exp.* != .string) {
        try p.errlog(arena, "import: invalid expression; expected string type");
    }

    const file_path = path_exp.string.value;
    if (name.len == 0) name = try p.findImportName(arena, file_path);
    const imput = std.fs.cwd().readFileAlloc(file_path, arena, .unlimited) catch |err| b: {
        try p.errlog(arena, @errorName(err));
        break :b "";
    };
    var new_lexer = Lexer.init(imput);
    // old state
    const file_name = p.errors.file;
    const last_lexer = p.lexer;
    const last_cur_token = p.cur_token;
    const last_peek_token = p.peek_token;
    const last_last_token = p.last_token;

    // new state
    p.errors.file = name;
    p.lexer = &new_lexer;
    p.cur_token = undefined;
    p.peek_token = undefined;
    p.last_token = .{ .tag = .eof };

    p.nextToken();
    p.nextToken();

    const node = try arena.create(ast.Node);
    node.* = try p.parse(arena);

    // back to old state
    p.errors.file = file_name;
    p.lexer = last_lexer;
    p.cur_token = last_cur_token;
    p.peek_token = last_peek_token;
    p.last_token = last_last_token;

    return .{
        .import = .{
            .name = .{
                .value = name,
            },
            // not used
            .path = path_exp,
            .token = tk,
            .node = node,
        },
    };
}

fn parseBreak(p: *Parser, arena: Allocator) !ast.Statement {
    var break_stmt: ast.Break = .{ .value = &NULL };

    if (p.peekTokenIs(.@"}")) {
        return .{ .@"break" = break_stmt };
    }

    p.nextToken();

    break_stmt.value = try p.parseExpression(arena, .lowest);

    if (p.peekTokenIs(.@";")) {
        p.nextToken();
    }

    return .{ .@"break" = break_stmt };
}

fn parseContinue(p: *Parser, arena: Allocator) !ast.Statement {
    var continue_stmt: ast.Continue = .{
        .value = &NULL,
    };

    if (p.peekTokenIs(.@"}")) {
        return .{ .@"continue" = continue_stmt };
    }

    p.nextToken();

    continue_stmt.value = try p.parseExpression(arena, .lowest);

    if (p.peekTokenIs(.@";")) {
        p.nextToken();
    }

    return .{ .@"continue" = continue_stmt };
}

fn parseAssignment(p: *Parser, arena: Allocator, name: *ast.Expression) !ast.Expression {
    var stmt: ast.Assignment = .{
        .name = name,
        .operator = undefined,
        .value = undefined,
    };

    stmt.name = name;

    const op = p.cur_token;

    p.nextToken();

    stmt.operator = switch (op.tag) {
        .@"=", .@":=", .@"+=", .@"-=", .@"/=", .@"*=" => op.tag,
        else => b: {
            try p.unexpected(arena, op.tag);
            try p.errlog(arena, "Invalid Operator");
            break :b op.tag;
        },
    };

    stmt.value = try p.parseExpression(arena, .lowest);

    return .{ .assignment = stmt };
}

pub fn parseDefer(p: *Parser, arena: Allocator) !ast.Statement {
    var defer_stmt: ast.Defer = .{
        .body = undefined,
    };

    if (!p.expectPeek(.@"{")) {
        try p.missing(arena, .@"{");
    }

    defer_stmt.body = try p.parseBlock(arena);

    return .{ .@"defer" = defer_stmt };
}
// con x, y, x = [1, 2, 3]
fn parseCon(p: *Parser, arena: Allocator) !ast.Statement {
    const tk = p.cur_token;

    // if (p.expectPeek( .@"(")) return .{ .con_block = try p.parseConBlock( tk) };

    var names: std.ArrayList(ast.Identifier) = .empty;
    errdefer names.deinit(arena);

    if (!p.expectPeek(.identifier)) {
        try p.missing(arena, .identifier);
    }

    try names.append(arena, p.parseIdentifier().identifier);

    // single identifier
    if (p.expectPeek(.@"=")) {
        p.nextToken();

        const value = try p.parseExpression(arena, .lowest);

        if (p.peekTokenIs(.@";")) p.nextToken();

        return .{ .con = .{
            .name = try names.toOwnedSlice(arena),
            .value = value,
            .token = tk,
        } };
    }

    //if (!p.expectPeek( .@",")) try p.missing(arena,  .@",");

    // multi identifiers
    while (p.expectPeek(.@",")) {
        if (!p.expectPeek(.identifier)) try p.missing(arena, .identifier);
        try names.append(arena, p.parseIdentifier().identifier);
    }

    if (!p.expectPeek(.@"=")) {
        try p.missing(arena, .@"=");
    }

    p.nextToken();

    // TODO: pointer?
    const value = try p.parseExpression(arena, .lowest);
    if (p.peekTokenIs(.@";")) p.nextToken();

    return .{ .con = .{
        .name = try names.toOwnedSlice(arena),
        .value = value,
        .token = tk,
    } };
}

fn parseVarBlock(p: *Parser, arena: Allocator, _: Token) !ast.VarBlock {
    const tk = p.cur_token;
    var stmt: ast.VarBlock = .{
        .vars_decl = undefined,
    };

    var vars: std.ArrayList(ast.Var) = .empty;
    errdefer vars.deinit(arena);

    p.nextToken();

    while (p.cur_token.tag != .@")") {
        const ident = p.parseIdentifier();

        if (ident != .identifier) try p.missing(arena, .identifier);

        p.nextToken();

        if (!p.expectPeek(.@"=")) try p.missing(arena, .@"=");

        p.nextToken();

        const exp = try p.parseExpression(arena, .lowest);

        const var_stmt = ast.Var{
            .name = ident.identifier,
            .value = exp,
            .token = tk,
        };

        try vars.append(arena, var_stmt);

        p.nextToken();
    }

    const vars_owned = try vars.toOwnedSlice(arena);

    stmt.vars_decl = vars_owned;

    return stmt;
}

fn parseVar(p: *Parser, arena: Allocator) !ast.Statement {
    const tk = p.cur_token;

    if (p.expectPeek(.@"(")) return .{ .var_block = try p.parseVarBlock(arena, tk) };

    var var_stmt: ast.Var = .{
        .name = undefined,
        .value = undefined,
        .token = tk,
    };

    if (!p.expectPeek(.identifier)) try p.missing(arena, .identifier);

    var_stmt.name = .{
        .value = p.tokenliteral(),
    };

    if (!p.expectPeek(.@"=")) {
        // const var_type = try p.parseExpression(arena, .lowest);

        p.nextToken();

        if (!p.expectPeek(.@"=")) {
            var_stmt.value = &NULL;

            if (p.peekTokenIs(.@";")) {
                p.nextToken();
            }

            return .{ .@"var" = var_stmt };
        }
    }

    p.nextToken();

    var_stmt.value = try p.parseExpression(arena, .lowest);

    if (p.peekTokenIs(.@";")) {
        p.nextToken();
    }

    return .{ .@"var" = var_stmt };
}

fn parseStatement(p: *Parser, arena: Allocator) !ast.Statement {
    const tk = p.cur_token;
    const stmt: ast.Statement = switch (tk.tag) {
        .@"var" => try p.parseVar(arena),

        .con => try p.parseCon(arena),

        .@"return" => try p.parseReturn(arena),

        .import => try p.parseImport(arena),

        .@"pub" => try p.parsePub(arena),

        .@"break" => try p.parseBreak(arena),

        .@"continue" => try p.parseContinue(arena),

        .@"fn" => try p.parseFn(arena),

        .@"defer" => try p.parseDefer(arena),

        .@"{" => .{ .block = try p.parseBlock(arena) },

        // .comment => p.parseComment(arena, tk),

        else => try p.parseExpStatement(arena),
    };

    return stmt;
}

// fn parseComment(_: *Parser, tk: Token) ast.Statement {
//     return .{ .comment = .{ .at = tk.at, .end = tk.end } };
// }

/// if, for, match, etc...
fn parseExpStatement(p: *Parser, arena: Allocator) !ast.Statement {
    const exp_stmt: ast.ExpStatement = .{
        .expression = try p.parseExpression(arena, .lowest),
        .token = p.cur_token,
    };

    if (p.peekTokenIs(.@";")) {
        p.nextToken();
    }

    return .{ .exp_statement = exp_stmt };
}

fn parseExpression(p: *Parser, arena: Allocator, precedence: Precedence) !*ast.Expression {
    const left_exp = try p.prefixExp(arena);

    while (@intFromEnum(precedence) < @intFromEnum(p.peekPrecedence())) {
        const old_left_exp = try arena.create(ast.Expression);
        errdefer arena.destroy(old_left_exp);
        old_left_exp.* = left_exp.*;

        left_exp.* = try infixExp(p, arena, old_left_exp) orelse {
            return left_exp;
        };
    }

    return left_exp;
}

pub fn parseProgram(p: *Parser, arena: Allocator) !ast.Program {
    var stmts: std.ArrayList(ast.Statement) = .empty;
    errdefer stmts.deinit(arena);

    var stmt: ast.Statement = undefined;
    while (p.cur_token.tag != .eof) {
        stmt = try p.parseStatement(arena);
        try stmts.append(arena, stmt);
        p.nextToken();
    }

    return .{ .statements = stmts };
}

pub fn parse(p: *Parser, arena: Allocator) !ast.Node {
    const program = try p.parseProgram(arena);
    return .{ .statement = .{ .program = program } };
}

fn tokenliteral(p: *const Parser) []const u8 {
    const loc = p.cur_token.loc;
    return p.lexer.input[loc.start..loc.end];
}

fn peektokenliteral(p: *const Parser) []const u8 {
    const loc = p.peek_token.loc;
    return p.lexer.input[loc.start..loc.end];
}

fn parseFloat(p: *const Parser) !ast.Expression {
    return .{ .float = .{
        .value = try std.fmt.parseFloat(f32, p.tokenliteral()),
    } };
}

fn parseChar(p: *Parser) ast.Expression {
    const literal = p.tokenliteral();
    var value: u8 = literal[0];

    if (literal.len > 1) {
        if (std.mem.eql(u8, literal, "\\n")) {
            value = '\n';
        } else if (std.mem.eql(u8, literal, "\\t")) {
            value = '\t';
        } else if (std.mem.eql(u8, literal, "\\r")) {
            value = '\r';
        }
    }

    return .{ .char = .{ .value = value } };
}

fn parseInteger(p: *Parser) !ast.Expression {
    return .{ .integer = .{
        .value = try std.fmt.parseInt(i32, p.tokenliteral(), 10),
    } };
}

fn parseIntegerFrom(arena: Allocator, i: usize) !*ast.Expression {
    const int = try arena.create(ast.Expression);
    errdefer arena.destroy(int);
    int.* = .{ .integer = .{ .value = @intCast(i) } };
    return int;
}

fn parseBoolean(p: *Parser) ast.Expression {
    return .{ .boolean = .{ .value = p.curTokenIs(.true) } };
}

fn parseNull(_: *Parser) ast.Expression {
    return .null;
}

//  BUG
fn parseGroup(p: *Parser, arena: Allocator) !ast.Expression {
    const incall = p.call;
    defer p.call = incall;
    p.call = false;

    p.nextToken();

    const exp = try p.parseExpression(arena, .lowest);

    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    return .{ .group = .{ .exp = exp } };
}

fn parsePrefix(p: *Parser, arena: Allocator) !ast.Expression {
    var expression: ast.Prefix = .{
        .operator = p.cur_token.tag,
        .right = undefined,
    };

    expression.operator = p.cur_token.tag;

    p.nextToken();

    expression.right = try p.parseExpression(arena, .prefix);

    return .{ .prefix = expression };
}

fn parseInfix(p: *Parser, arena: Allocator, left: *ast.Expression) !ast.Expression {
    var infix = ast.Infix{
        .operator = p.cur_token.tag,
        .left = left,
        .right = undefined,
    };

    const precedence = p.currentPrecedence();

    p.nextToken();

    infix.right = try p.parseExpression(arena, precedence);

    return .{ .infix = infix };
}

fn parseIf(p: *Parser, arena: Allocator) !ast.Expression {
    var if_exp: ast.If = .{
        .condition = undefined,
        .consequence = undefined,
    };

    if (!p.expectPeek(.@"(")) try p.missing(arena, .@"(");
    p.nextToken();

    if_exp.condition = try p.parseExpression(arena, .lowest);

    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    if_exp.consequence = if (p.expectPeek(.@"{"))
        try p.parseBlock(arena)
    else
        try p.parseSingleStmtBlock(arena);

    if (p.expectPeek(.@"else")) {
        if_exp.alternative = if (p.expectPeek(.@"{"))
            try p.parseBlock(arena)
        else
            try p.parseSingleStmtBlock(arena);
    }

    return .{ .@"if" = if_exp };
}

// TODO: allow single expression blocks to optionally ignore braces
fn parseBlock(p: *Parser, arena: Allocator) anyerror!ast.Block {
    var stmts: std.ArrayList(ast.Statement) = .empty;
    errdefer stmts.deinit(arena);

    // : or {
    const tk = p.cur_token;
    var block: ast.Block = .{
        .token = tk,
        .statements = undefined,
    };

    p.nextToken();

    if (tk.tag == .@":") {
        const stmt = try p.parseStatement(arena);
        try stmts.append(arena, stmt);
        // p.nextToken(, );
    } else {
        while (!p.curTokenIs(.@"}") and !p.curTokenIs(.eof)) {
            const stmt = try p.parseStatement(arena);
            try stmts.append(arena, stmt);
            p.nextToken();
        }

        if (!p.curTokenIs(.@"}")) try p.missing(arena, .@"}");
    }

    const stmts_owner = try stmts.toOwnedSlice(arena);

    block.statements = stmts_owner;

    return block;
}

fn parseSingleStmtBlock(p: *Parser, arena: Allocator) anyerror!ast.Block {
    var stmts: std.ArrayList(ast.Statement) = .empty;
    errdefer stmts.deinit(arena);
    const tk = p.cur_token;
    var block: ast.Block = .{
        .token = tk,
        .statements = undefined,
    };
    p.nextToken();

    const stmt = try p.parseStatement(arena);
    try stmts.append(arena, stmt);
    const stmts_owner = try stmts.toOwnedSlice(arena);
    block.statements = stmts_owner;
    return block;
}

fn parseFn(p: *Parser, arena: Allocator) !ast.Statement {
    var func_stmt: ast.FunctionStatement = .{
        .func = undefined,
        .name = undefined,
        .token = p.cur_token,
    };

    if (!p.expectPeek(.identifier)) {
        try p.missing(arena, .identifier);
    }

    func_stmt.name = p.parseIdentifier().identifier;

    func_stmt.func = (try p.parseFunction(arena)).function;

    return .{ .@"fn" = func_stmt };
}

fn parseType(p: *Parser, arena: Allocator) !ast.Expression {
    var struc_or_enum: ast.Type = .{ .type = p.cur_token.tag };

    if (!p.expectPeek(.@"{")) try p.missing(arena, .@"{");

    var fields: std.ArrayList(ast.Type.Field) = .empty;
    errdefer fields.deinit(arena);

    var decls: std.ArrayList(ast.FunctionStatement) = .empty;
    errdefer decls.deinit(arena);

    var names: std.StringHashMap(void) = .init(arena);
    defer names.deinit();

    while (!p.peekTokenIs(.@"}") and !p.curTokenIs(.eof)) {
        p.nextToken();

        if (p.curTokenIs(.identifier)) {
            const name = p.parseIdentifier().identifier;

            if (names.contains(name.value)) {
                try p.errors.append(arena, "Duplicated field name {s}\n", .{name.value});
            }

            try names.put(name.value, {});

            var value = &NULL;
            if (p.expectPeek(.@"=")) {
                p.nextToken();
                value = try p.parseExpression(arena, .lowest);
            }

            if (p.peekTokenIs(.@":")) try p.unexpected(arena, .@":");

            if (p.peekTokenIs(.@",") or p.curTokenIs(.@";")) {
                p.nextToken();
            }

            try fields.append(arena, .{ .name = name, .value = value });
            continue;
        }

        if (p.curTokenIs(.@"fn")) {
            const tk = p.cur_token;
            if (!p.expectPeek(.identifier)) {
                try p.missing(arena, .identifier);
            }

            const name = p.parseIdentifier().identifier;

            if (names.contains(name.value)) {
                try p.errors.append(arena, "Duplicated field name {s}\n", .{name.value});
            }

            try names.put(name.value, {});

            const func = (try p.parseFunction(arena)).function;

            try decls.append(arena, .{
                .name = name,
                .func = func,
                .token = tk,
            });
            continue;
        }

        // // TODO: BIG FIX
        // if (p.curTokenIs( .comment)) {
        //     try comments.append(arena, p.parseComment(arena, p.cur_token));
        //     continue;
        // }

        try p.errlog(arena, "Invalid Struct");
        return .bad;
    }

    p.nextToken();

    if (!p.curTokenIs(.@"}")) try p.missing(arena, .@"}");

    struc_or_enum.fields = try fields.toOwnedSlice(arena);
    struc_or_enum.decl = try decls.toOwnedSlice(arena);
    // struc.comments = try comments.toOwnedSlice(arena);

    return .{ .type = struc_or_enum };
}

fn parseFunction(p: *Parser, arena: Allocator) !ast.Expression {
    var func: ast.Function = .{
        .parameters = undefined,
        .body = undefined,
    };

    if (!p.expectPeek(.@"(")) try p.missing(arena, .@"(");

    func.parameters = try p.parseFunctionParameters(arena);

    if (!p.expectPeek(.@"{")) try p.missing(arena, .@"{");

    func.body = try p.parseBlock(arena);

    return .{ .function = func };
}

fn parseFunctionParameters(p: *Parser, arena: Allocator) ![]ast.Identifier {
    var indentifiers: std.ArrayList(ast.Identifier) = .empty;
    errdefer indentifiers.deinit(arena);

    if (p.peekTokenIs(.@")")) {
        p.nextToken();
        return try indentifiers.toOwnedSlice(arena);
    }

    p.nextToken();

    const ident: ast.Identifier = .{
        .value = p.tokenliteral(),
    };

    try indentifiers.append(arena, ident);

    while (p.peekTokenIs(.@",")) {
        p.nextToken();
        p.nextToken();

        const ident2 = p.parseIdentifier().identifier;

        try indentifiers.append(arena, ident2);
    }

    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    const ident_owner = try indentifiers.toOwnedSlice(arena);

    return ident_owner;
}

fn parseMethod(p: *Parser, arena: Allocator, caller: *ast.Expression) !ast.Expression {
    var method_exp: ast.Method = .{
        .caller = caller,
        .method = undefined,
    };

    p.nextToken();

    const exp_mathod = p.parseIdentifier();

    if (exp_mathod != .identifier) try p.missing(arena, .identifier);

    method_exp.method = exp_mathod.identifier;

    // std.debug.print("{}", .{p.peekTokenIs( .@"(")});

    return .{ .method = method_exp };
}

fn parseInstance(p: *Parser, arena: Allocator, type_exp: *ast.Expression) !ast.Expression {
    const incall = p.call;
    defer p.call = incall;
    p.call = true;
    return .{
        .instance = .{ .type = type_exp, .fields = try p.parseInstanceArguments(arena) },
    };
}

fn parseInstance3(p: *Parser, arena: Allocator) !ast.Expression {
    return .{ .instance = .{
        .type = try p.parseExpression(arena, .lowest),
        .fields = try p.parseInstanceArguments(arena),
    } };
}

fn parseInstance2(p: *Parser, arena: Allocator) !ast.Expression {
    p.nextToken();
    return .{ .instance = .{
        .type = try p.parseExpression(arena, .lowest),
        .fields = try p.parseInstanceArguments(arena),
    } };
}

fn parseInstanceArguments(p: *Parser, arena: Allocator) ![]ast.Type.Field {
    var fields: std.ArrayList(ast.Type.Field) = .empty;
    errdefer fields.deinit(arena);

    if (!p.peekTokenIs(.@"{")) {
        try p.missing(arena, .@"{");
    }

    p.nextToken();

    if (p.peekTokenIs(.@"}")) {
        p.nextToken();
        return try fields.toOwnedSlice(arena);
    }

    while (!p.peekTokenIs(.@"}") and !p.curTokenIs(.eof)) {
        p.nextToken();

        if (p.curTokenIs(.identifier)) {
            var field: ast.Type.Field = .{
                .name = p.parseIdentifier().identifier,
                .value = &NULL,
            };

            if (p.expectPeek(.@":")) {
                p.nextToken();
                field.value = try p.parseExpression(arena, .lowest);
            }

            try fields.append(arena, field);

            if (!p.expectPeek(.@",")) {
                if (p.peekTokenIs(.@"}")) break;
                try p.missing(arena, .@",");
            }

            continue;
        }

        try p.errlog(arena, "Syntax error: Invalid Initialization");
    }

    p.nextToken();

    if (!p.curTokenIs(.@"}")) try p.missing(arena, .@"}");

    return fields.toOwnedSlice(arena);
}

fn parseCall(p: *Parser, arena: Allocator, func: *ast.Expression) !ast.Expression {
    return .{ .call = .{ .function = func, .arguments = try p.parseCallArguments(arena) } };
}

fn parseCallArguments(p: *Parser, arena: Allocator) ![]*ast.Expression {
    var args: std.ArrayList(*ast.Expression) = .empty;
    errdefer args.deinit(arena);

    if (p.peekTokenIs(.@")")) {
        p.nextToken();
        return try args.toOwnedSlice(arena);
    }

    p.nextToken();

    const exp = try p.parseExpression(arena, .lowest);
    try args.append(arena, exp);

    while (p.peekTokenIs(.@",")) {
        p.nextToken();
        p.nextToken();

        const exp2 = try p.parseExpression(arena, .lowest);
        try args.append(arena, exp2);
    }

    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    const args_owned = try args.toOwnedSlice(arena);

    return args_owned;
}

fn parseTuple(p: *Parser, arena: Allocator, exp: *ast.Expression) !*ast.Expression {
    const tuple_exp = try arena.create(ast.Expression);
    errdefer arena.destroy(tuple_exp);
    tuple_exp.* = .{ .tuple = .{ .elements = try p.parseExpList(arena, exp) } };
    return tuple_exp;
}

fn newExp(p: *Parser, arena: Allocator) anyerror!*ast.Expression {
    const left_exp = try arena.create(ast.Expression);
    errdefer arena.destroy(left_exp);
    left_exp.* = switch (p.cur_token.tag) {
        .identifier => p.parseIdentifier(),
        .@"." => p.parseTag(),
        .integer => try p.parseInteger(),
        .float => try p.parseFloat(),
        .true, .false => p.parseBoolean(),
        .null, .@";" => p.parseNull(),
        .@"[" => try p.parseArray(arena),
        .@"{" => try p.parseHash(arena),
        .@"fn" => try p.parseFunction(arena),
        .@"struct", .@"enum" => try p.parseType(arena),
        .string => p.parseString(),
        .char => p.parseChar(),
        .@"if", .@"else" => try p.parseIf(arena),
        .@"for" => try p.parseFor(arena),
        .match => try p.parseMatch(arena),
        .@"!", .@"-" => try p.parsePrefix(arena),
        .@"(" => try p.parseGroup(arena),
        else => b: {
            try p.unexpected(arena, p.cur_token.tag);
            break :b .bad;
        },
    };
    return left_exp;
}

fn parseExpList(p: *Parser, arena: Allocator, exp1: *ast.Expression) ![]*ast.Expression {
    var exps: std.ArrayList(*ast.Expression) = .empty;
    errdefer exps.deinit(arena);

    try exps.append(arena, exp1);

    while (p.peekTokenIs(.@",")) {
        p.nextToken();
        p.nextToken();

        const left_exp = try p.newExp(arena);
        try exps.append(arena, left_exp);
    }

    return try exps.toOwnedSlice(arena);
}

fn parseMatchArm(p: *Parser, arena: Allocator) ![]*ast.Expression {
    const incall = p.call;
    defer p.call = incall;
    p.call = true;

    var args: std.ArrayList(*ast.Expression) = .empty;
    errdefer args.deinit(arena);

    const exp = try p.parseExpression(arena, .lowest);
    try args.append(arena, exp);

    while (p.peekTokenIs(.@",")) {
        p.nextToken();
        p.nextToken();

        const exp2 = try p.parseExpression(arena, .lowest);
        try args.append(arena, exp2);
    }

    if (!p.expectPeek(.@"=>")) try p.missing(arena, .@"=>");

    const args_owned = try args.toOwnedSlice(arena);

    return args_owned;
}

pub fn parseString(p: *Parser) ast.Expression {
    return .{
        .string = .{
            .value = p.tokenliteral(),
        },
    };
}

fn debugCurToken(p: *Parser) void {
    std.debug.print("{s}\n", .{p.tokenliteral()});
}

pub fn parseArray(p: *Parser, arena: Allocator) !ast.Expression {
    if (p.peekTokenIs(.@"]")) {
        p.nextToken();
        return .{
            .array = .{ .elements = &.{} },
        };
    }

    var elements: std.ArrayList(*ast.Expression) = .empty;
    errdefer elements.deinit(arena);

    p.nextToken();

    const incall = p.call;
    defer p.call = incall;
    p.call = true;
    const element1 = try p.parseExpression(arena, .lowest);
    try elements.append(arena, element1);

    while (p.expectPeek(.@",")) {
        p.nextToken(); // next_element

        const element_n = try p.parseExpression(arena, .lowest);
        try elements.append(arena, element_n);
    }

    if (!p.expectPeek(.@"]")) try p.missing(arena, .@"]");
    return .{
        .array = .{ .elements = try elements.toOwnedSlice(arena) },
    };
}

// var hash = {1:1, 2:2, 3:3}
pub fn parseHash(p: *Parser, arena: Allocator) !ast.Expression {
    const incall = p.call;
    defer p.call = incall;
    p.call = true;

    var pairs: std.ArrayList([2]*ast.Expression) = .empty;
    errdefer pairs.deinit(arena);

    var counter: usize = 0;
    while (!p.peekTokenIs(.@"}")) {
        p.nextToken();
        const key = try p.parseExpression(arena, .lowest);

        if (!p.expectPeek(.@":")) {
            if (p.expectPeek(.@",") or p.expectPeek(.@"}")) {
                const index = try parseIntegerFrom(arena, counter);
                counter += 1;
                // try pairs.put(index, key);
                try pairs.append(arena, .{ index, key });

                if (p.curTokenIs(.@"}")) {
                    return .{ .hash = .{ .pairs = try pairs.toOwnedSlice(arena) } };
                }

                continue;
            }

            try p.missing(arena, .@":");
        }

        p.nextToken();
        const val = try p.parseExpression(arena, .lowest);

        try pairs.append(arena, .{ key, val });

        if (!p.peekTokenIs(.@"}") and !p.expectPeek(.@",")) {
            try p.missing(arena, .@",");
            try p.missing(arena, .@"}");
        }
    }

    if (!p.expectPeek(.@"}")) try p.missing(arena, .@"}");

    return .{ .hash = .{ .pairs = try pairs.toOwnedSlice(arena) } };
}

// v[exp]
pub fn parseIndex(p: *Parser, arena: Allocator, left: *ast.Expression) !ast.Expression {
    var exp: ast.Index = .{
        .left = left,
        .index = undefined,
    };

    p.nextToken();

    exp.index = try p.parseExpression(arena, .lowest);

    if (!p.expectPeek(.@"]")) try p.missing(arena, .@"]");

    return .{ .index = exp };
}

pub fn parseMatch(p: *Parser, arena: Allocator) !ast.Expression {
    var match: ast.Match = .{
        .value = undefined,
        .arms = undefined,
    };

    p.nextToken();

    // BUG match () {}
    if (p.curTokenIs(.@"{")) {
        match.value = &TRUE;
    } else {
        if (!p.curTokenIs(.@"(")) try p.missing(arena, .@"(");
        p.nextToken();
        match.value = try p.parseExpression(arena, .lowest);
        if (!p.expectPeek(.@")")) try p.missing(arena, .@")");
        if (!p.expectPeek(.@"{")) try p.missing(arena, .@"{");
    }

    p.nextToken();

    var arms: std.ArrayList(ast.Match.Arm) = .empty;
    errdefer arms.deinit(arena);

    while (!p.curTokenIs(.@"}") and !p.curTokenIs(.eof)) {

        // the else arm
        if (p.curTokenIs(.@"else")) {
            if (match.else_block != null) try p.errlog(arena, "Duplicated Tag");

            if (!p.expectPeek(.@"=>")) try p.missing(arena, .@"=>");

            match.else_block = if (p.expectPeek(.@"{"))
                try p.parseBlock(arena)
            else
                try p.parseSingleStmtBlock(arena);

            p.nextToken();
            continue;
        }

        var arm: ast.Match.Arm = .{
            .condition = try p.parseMatchArm(arena),
            .block = undefined,
        };

        // if (!p.expectPeek( .@"=>")) try p.missing(arena,  .@"=>");

        arm.block = if (p.expectPeek(.@"{"))
            try p.parseBlock(arena)
        else
            try p.parseSingleStmtBlock(arena);

        p.nextToken();
        try arms.append(arena, arm);
    }

    match.arms = try arms.toOwnedSlice(arena);

    return .{ .match = match };
}

pub fn parseRange(p: *Parser, arena: Allocator, left: *ast.Expression) !ast.Expression {
    var range = ast.Range{
        .start = left,
        .end = &NULL,
        .inc = if (p.curTokenIs(.@"..")) .no else .yes,
    };

    if (p.peekTokenIs(.@")") or p.peekTokenIs(.@"]")) {
        return .{ .range = range };
    }

    p.nextToken();

    range.end = try p.parseExpression(arena, .lowest);

    return .{ .range = range };
}

pub fn parseForRange(p: *Parser, arena: Allocator, ident: ast.Identifier) !ast.Expression {
    var flr = ast.ForRange{
        .ident = ident.value, // for <ident>[, <idx>] in <range> {
        .body = undefined,
        .iterable = undefined,
    };

    if (p.expectPeek(.@",")) {
        p.nextToken();
        const index = p.parseIdentifier();
        flr.index = index.identifier.value;
    }

    if (!p.expectPeek(.in)) {
        try p.missing(arena, .in);
    }

    p.nextToken();

    flr.iterable = try p.parseExpression(arena, .lowest);
    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    if (p.expectPeek(.@"{")) {
        flr.body = try p.parseBlock(arena);
    } else {
        flr.body = try p.parseSingleStmtBlock(arena);
    }

    return .{ .for_range = flr };
}

pub fn parseForLoopCondition(p: *Parser, arena: Allocator, cond: *ast.Expression) !ast.Expression {
    var fl = ast.For{
        .condition = cond,
        .consequence = undefined,
    };

    if (p.expectPeek(.@"{")) {
        fl.consequence = try p.parseBlock(arena);
    } else {
        fl.consequence = try p.parseSingleStmtBlock(arena);
    }

    return .{ .@"for" = fl };
}

// /// for true { } or for idx, val in list {}
pub fn parseFor(p: *Parser, arena: Allocator) !ast.Expression {
    if (p.expectPeek(.@"{")) {
        return .{ .@"for" = .{ .condition = &TRUE, .consequence = try p.parseBlock(arena) } };
    }

    if (!p.expectPeek(.@"(")) try p.missing(arena, .@"(");
    p.nextToken();

    const incall = p.call;
    p.call = true;
    const condition_or_ident = try p.parseExpression(arena, .lowest);
    p.call = incall;

    if (condition_or_ident.* == .identifier) {
        if (p.peekTokenIs(.in) or p.peekTokenIs(.@",")) {
            return p.parseForRange(arena, condition_or_ident.identifier);
        }
    }

    if (!p.expectPeek(.@")")) try p.missing(arena, .@")");

    return p.parseForLoopCondition(arena, condition_or_ident);
}

/// TODO: what about pear type resolution?
fn missing(p: *Parser, arena: Allocator, tk: Token.Tag) !void {
    const l = p.line();
    const lin = l.line;
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        arena,
        "\x1b[1m{s}:{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m Expected '{s}', found '{s}' ({s}) \x1b[0m\n\t{s}\n\t",
        .{ p.errors.file, p.lexer.line_index, at, @tagName(tk), p.peektokenliteral(), @tagName(p.peek_token.tag), lin },
    );
    // try p.errors.msg.writer().writeByteNTimes(' ', at - 1);
    try p.errors.append(arena, "\x1b[32m^\x1b[0m\n", .{});
}

fn unexpected(p: *Parser, arena: Allocator, tk: Token.Tag) !void {
    const l = p.line();
    const lin = l.line;
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        arena,
        "\x1b[1m{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m Unexpected token '{s}'\x1b[0m\n\t{s}\n\t",
        .{ p.lexer.line_index, at, @tagName(tk), lin },
    );
    // try p.errors.msg.writer().writeByteNTimes(' ', at - 1);
    try p.errors.append(arena, "\x1b[32m^\x1b[0m\n", .{});
}

fn errlog(p: *Parser, arena: Allocator, msg: []const u8) !void {
    const l = p.line();
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        arena,
        "\x1b[1m{s}:{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m {s} \x1b[0m\n",
        .{ p.errors.file, p.lexer.line_index, at, msg },
    );
}

const Line = struct {
    line: []const u8,
    start: usize,
    end: usize,
};

fn line(p: *Parser) Line {
    const input = p.lexer.input;
    var pos = p.lexer.position;
    if (pos >= input.len) {
        pos = p.lexer.read_position;
        if (pos >= input.len) {
            pos = input.len;
        }
    }

    const start = std.mem.lastIndexOf(u8, input[0..pos], "\n") orelse 0;
    var end = std.mem.indexOf(u8, input[pos..], "\n") orelse input.len;
    if (end != input.len) end += pos;

    return .{ .line = std.mem.trim(u8, input[start..end], "\n"), .end = end, .start = start };
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
    // tuple,

    pub fn peek(token_type: Token.Tag) Precedence {
        return switch (token_type) {
            else => .lowest,
            .@"or" => .lor,
            .@"and" => .land,
            .@"==", .@"!=" => .equals,
            .@">", .@">=", .@"<", .@"<=" => .lessgreater,
            .@"+", .@"-" => .sum,
            .@"/", .@"*", .@"%", .@"^" => .product,
            .@"(", .@"{" => .call,
            .@"!" => .prefix,
            .@"[", .@".", .@"..", .@"..=" => .index,
            .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
            // .@"," => .tuple,
        };
    }
};
