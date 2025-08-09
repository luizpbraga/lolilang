const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
const Parser = @This();
const File = @import("Line.zig");
const Error = @import("Error.zig");

lexer: *Lexer,
cur_token: Token,
last_token: Token,
peek_token: Token,
arena: std.heap.ArenaAllocator,
errors: *Error,
call: bool = false,
types: std.StringHashMap(*ast.Expression),

pub fn init(child_alloc: std.mem.Allocator, lexer: *Lexer, errors: *Error) Parser {
    var arena: std.heap.ArenaAllocator = .init(child_alloc);
    const allocator = arena.allocator();

    var parser: Parser = .{
        .arena = arena,
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .last_token = .{ .tag = .eof },
        .errors = errors,
        .types = .init(allocator),
    };

    parser.nextToken();
    parser.nextToken();

    return parser;
}

pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}

fn prefixExp(p: *Parser) anyerror!*ast.Expression {
    const allocator = p.arena.allocator();
    const left_exp = try p.newExp(allocator);

    if (!p.call and p.peekTokenIs(.@",")) {
        return p.parseTuple(left_exp);
    }

    return left_exp;
}

fn infixExp(p: *Parser, lx: *ast.Expression) anyerror!?ast.Expression {
    var tk: Token = p.peek_token;
    const left_exp: ?ast.Expression = b: switch (tk.tag) {
        .@"[" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseIndex(lx);
        },
        .@"..", .@"..=" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseRange(lx);
        },
        .@"." => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseMethod(lx);
        },
        .@"{" => {
            // p.nextToken();
            tk = p.cur_token;
            break :b try p.parseInstance(lx);
        },
        .@"(" => {
            p.nextToken();
            tk = p.cur_token;
            p.call = true;
            defer p.call = false;
            break :b try p.parseCall(lx);
        },

        .@"%", .@"+", .@"-", .@"==", .@"!=", .@"*", .@"or", .@"and", .@"/", .@">", .@">=", .@"^" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseInfix(lx);
        },

        .@"<", .@"<=" => {
            // a < b < c
            // a < b and b < c
            p.nextToken();
            tk = p.cur_token;
            const infix_left = try p.parseInfix(lx);

            // CLASSIC a < b OR a <= b
            if (!p.expectPeek(.@"<") and !p.expectPeek(.@"<=")) {
                break :b infix_left;
            }

            // FANCY a < b < c
            tk = p.cur_token;
            const infix_rigth = try p.parseInfix(infix_left.infix.right);

            const l = try p.arena.allocator().create(ast.Expression);
            const r = try p.arena.allocator().create(ast.Expression);
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
            break :b try p.parseAssignment(lx);
        },

        else => break :b null,
    };

    return left_exp;
}

fn nextToken(self: *Parser) void {
    self.last_token = self.cur_token;
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn curTokenIs(self: *const Parser, token_type: Token.Tag) bool {
    return self.cur_token.tag == token_type;
}

fn peekTokenIs(self: *const Parser, token_type: Token.Tag) bool {
    return self.peek_token.tag == token_type;
}

/// match and eat the peeked token (if true)
fn expectPeek(self: *Parser, token_type: Token.Tag) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    } else {
        return false;
    }
}

fn peekPrecedence(self: *const Parser) Precedence {
    return .peek(self.peek_token.tag);
}

fn currentPrecedence(self: *const Parser) Precedence {
    return .peek(self.cur_token.tag);
}

// parsers fn -----------------------------------------------------------
fn parseIdentifier(self: *const Parser) ast.Expression {
    return .{ .identifier = .{
        .value = self.tokenliteral(),
    } };
}

fn parseTag(self: *Parser) ast.Expression {
    self.nextToken();
    return .{ .tag = .{
        .value = self.tokenliteral(),
    } };
}

fn parseReturn(self: *Parser) !ast.Statement {
    var return_stmt: ast.Return = .{ .value = &NULL, .token = self.cur_token };

    if (self.peekTokenIs(.@"}")) {
        return .{ .@"return" = return_stmt };
    }

    self.nextToken();

    var value = try self.parseExpression(.lowest);
    if (self.peekTokenIs(.@",")) {
        value = try self.parseTuple(value);
    }

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return_stmt.value = value;

    return .{ .@"return" = return_stmt };
}

fn parsePub(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;
    self.nextToken();

    const func_or_var = try self.parseStatement();

    switch (func_or_var) {
        .@"fn", .@"var", .import => {},
        else => try self.errlog("Invalid public declaration"),
    }

    const stmt = try self.arena.allocator().create(ast.Statement);
    stmt.* = func_or_var;

    return .{ .@"pub" = .{ .token = tk, .stmt = stmt } };
}

fn findImportName(p: *Parser, path: []const u8) ![]const u8 {
    if (!std.mem.endsWith(u8, path, ".loli")) {
        try p.errlog("Invalid Module Path: expected .loli extention, got your mama");
    }
    var start = std.mem.lastIndexOf(u8, path, "/") orelse 0;
    if (start != 0) start += 1;
    const end = std.mem.lastIndexOf(u8, path, ".") orelse return error.InvalidPath;
    return path[start..end];
}

// import "fmt"
fn parseImport(self: *Parser) anyerror!ast.Statement {
    const tk = self.cur_token;
    self.nextToken();

    var name: []const u8 = "";
    if (self.curTokenIs(.identifier)) {
        name = self.tokenliteral();
        self.nextToken();
    }

    const path_exp = try self.parseExpression(.lowest);

    if (path_exp.* != .string) {
        try self.errlog("import: invalid expression; expected string type");
    }

    const file_path = path_exp.string.value;
    if (name.len == 0) name = try self.findImportName(file_path);
    const imput = std.fs.cwd().readFileAlloc(self.arena.allocator(), file_path, std.math.maxInt(usize)) catch |err| b: {
        try self.errlog(@errorName(err));
        break :b "";
    };
    var new_lexer = Lexer.init(imput);
    // old state
    const file_name = self.errors.file;
    const last_lexer = self.lexer;
    const last_cur_token = self.cur_token;
    const last_peek_token = self.peek_token;
    const last_last_token = self.last_token;

    // new state
    self.errors.file = name;
    self.lexer = &new_lexer;
    self.cur_token = undefined;
    self.peek_token = undefined;
    self.last_token = .{ .tag = .eof };

    self.nextToken();
    self.nextToken();

    const node = try self.arena.allocator().create(ast.Node);
    node.* = try self.parse();

    // back to old state
    self.errors.file = file_name;
    self.lexer = last_lexer;
    self.cur_token = last_cur_token;
    self.peek_token = last_peek_token;
    self.last_token = last_last_token;

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

fn parseBreak(self: *Parser) !ast.Statement {
    var break_stmt: ast.Break = .{ .value = &NULL };

    if (self.peekTokenIs(.@"}")) {
        return .{ .@"break" = break_stmt };
    }

    self.nextToken();

    break_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"break" = break_stmt };
}

fn parseContinue(self: *Parser) !ast.Statement {
    var continue_stmt: ast.Continue = .{
        .value = &NULL,
    };

    if (self.peekTokenIs(.@"}")) {
        return .{ .@"continue" = continue_stmt };
    }

    self.nextToken();

    continue_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"continue" = continue_stmt };
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

    stmt.operator = switch (op.tag) {
        .@"=", .@":=", .@"+=", .@"-=", .@"/=", .@"*=" => op.tag,
        else => b: {
            try self.unexpected(op.tag);
            try self.errlog("Invalid Operator");
            break :b op.tag;
        },
    };

    stmt.value = try self.parseExpression(.lowest);

    // TODO: WHYYYY IT DOES NOT WOOORK?
    if (stmt.operator == .@":=" and (stmt.value.* == .type or stmt.value.* == .class)) {
        try self.types.put(name.identifier.value, stmt.value);
        stmt.value.class.name = name.identifier.value;
    }
    if (stmt.operator == .@":=" and stmt.value.* == .instance) {
        stmt.value.instance.type.class.name = name.identifier.value;
    }

    return .{ .assignment = stmt };
}

pub fn parseDefer(self: *Parser) !ast.Statement {
    var defer_stmt: ast.Defer = .{
        .body = undefined,
    };

    if (!self.expectPeek(.@"{")) {
        try self.missing(.@"{");
    }

    defer_stmt.body = try self.parseBlock();

    return .{ .@"defer" = defer_stmt };
}
// con x, y, x = [1, 2, 3]
fn parseCon(self: *Parser) !ast.Statement {
    const tk = self.cur_token;

    // if (self.expectPeek(.@"(")) return .{ .con_block = try self.parseConBlock(tk) };

    var names: std.ArrayList(ast.Identifier) = .init(self.arena.allocator());
    errdefer names.deinit();

    if (!self.expectPeek(.identifier)) {
        try self.missing(.identifier);
    }

    try names.append(self.parseIdentifier().identifier);

    // single identifier
    if (self.expectPeek(.@"=")) {
        self.nextToken();

        const value = try self.parseExpression(.lowest);

        if (self.peekTokenIs(.@";")) self.nextToken();

        return .{ .con = .{
            .name = try names.toOwnedSlice(),
            .value = value,
            .token = tk,
        } };
    }

    //if (!self.expectPeek(.@",")) try self.missing(.@",");

    // multi identifiers
    while (self.expectPeek(.@",")) {
        if (!self.expectPeek(.identifier)) try self.missing(.identifier);
        try names.append(self.parseIdentifier().identifier);
    }

    if (!self.expectPeek(.@"=")) {
        try self.missing(.@"=");
    }

    self.nextToken();

    // TODO: pointer?
    const value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) self.nextToken();

    return .{ .con = .{
        .name = try names.toOwnedSlice(),
        .value = value,
        .token = tk,
    } };
}

fn parseVarBlock(self: *Parser, _: Token) !ast.VarBlock {
    const tk = self.cur_token;
    var stmt: ast.VarBlock = .{
        .vars_decl = undefined,
    };

    const allocator = self.arena.allocator();

    var vars: std.ArrayList(ast.Var) = .init(allocator);
    errdefer vars.deinit();

    self.nextToken();

    while (self.cur_token.tag != .@")") {
        const ident = self.parseIdentifier();

        if (ident != .identifier) try self.missing(.identifier);

        self.nextToken();

        if (!self.expectPeek(.@"=")) try self.missing(.@"=");

        self.nextToken();

        const exp = try self.parseExpression(.lowest);

        const var_stmt = ast.Var{
            .name = ident.identifier,
            .value = exp,
            .token = tk,
        };

        try vars.append(var_stmt);

        self.nextToken();
    }

    const vars_owned = try vars.toOwnedSlice();

    stmt.vars_decl = vars_owned;

    return stmt;
}

fn parseVar(self: *Parser) !ast.Statement {
    const tk = self.cur_token;

    if (self.expectPeek(.@"(")) return .{ .var_block = try self.parseVarBlock(tk) };

    var var_stmt: ast.Var = .{
        .name = undefined,
        .value = undefined,
        .token = tk,
    };

    if (!self.expectPeek(.identifier)) try self.missing(.identifier);

    var_stmt.name = .{
        .value = self.tokenliteral(),
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

    var_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    if (var_stmt.value.* == .type or var_stmt.value.* == .class) try self.types.put(var_stmt.name.value, var_stmt.value);

    return .{ .@"var" = var_stmt };
}

fn parseStatement(self: *Parser) !ast.Statement {
    const tk = self.cur_token;
    const stmt: ast.Statement = switch (tk.tag) {
        .@"var" => try self.parseVar(),

        .con => try self.parseCon(),

        .@"return" => try self.parseReturn(),

        .import => try self.parseImport(),

        .@"pub" => try self.parsePub(),

        .@"break" => try self.parseBreak(),

        .@"continue" => try self.parseContinue(),

        .@"fn" => try self.parseFn(),

        .@"defer" => try self.parseDefer(),

        .@"{" => .{ .block = try self.parseBlock() },

        // .comment => self.parseComment(tk),

        else => try self.parseExpStatement(),
    };

    return stmt;
}

// fn parseComment(_: *Parser, tk: Token) ast.Statement {
//     return .{ .comment = .{ .at = tk.at, .end = tk.end } };
// }

/// if, for, match, etc...
fn parseExpStatement(self: *Parser) !ast.Statement {
    const exp_stmt: ast.ExpStatement = .{
        .expression = try self.parseExpression(.lowest),
        .token = self.cur_token,
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
    while (self.cur_token.tag != .eof) {
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

fn tokenliteral(self: *const Parser) []const u8 {
    const loc = self.cur_token.loc;
    return self.lexer.input[loc.start..loc.end];
}

fn peektokenliteral(self: *const Parser) []const u8 {
    const loc = self.peek_token.loc;
    return self.lexer.input[loc.start..loc.end];
}

fn parseFloat(self: *const Parser) !ast.Expression {
    return .{ .float = .{
        .value = try std.fmt.parseFloat(f32, self.tokenliteral()),
    } };
}

fn parseChar(self: *Parser) ast.Expression {
    const literal = self.tokenliteral();
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

fn parseInteger(self: *Parser) !ast.Expression {
    return .{ .integer = .{
        .value = try std.fmt.parseInt(i32, self.tokenliteral(), 10),
    } };
}

fn parseIntegerFrom(self: *Parser, i: usize) !*ast.Expression {
    const alloc = self.arena.allocator();
    const int = try alloc.create(ast.Expression);
    errdefer alloc.destroy(int);
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
    const incall = self.call;
    defer self.call = incall;
    self.call = false;

    self.nextToken();

    const exp = try self.parseExpression(.lowest);

    if (!self.expectPeek(.@")")) try self.missing(.@")");

    return .{ .group = .{ .exp = exp } };
}

fn parsePrefix(self: *Parser) !ast.Expression {
    var expression: ast.Prefix = .{
        .operator = self.cur_token.tag,
        .right = undefined,
    };

    expression.operator = self.cur_token.tag;

    self.nextToken();

    expression.right = try self.parseExpression(.prefix);

    return .{ .prefix = expression };
}

fn parseInfix(self: *Parser, left: *ast.Expression) !ast.Expression {
    var infix = ast.Infix{
        .operator = self.cur_token.tag,
        .left = left,
        .right = undefined,
    };

    const precedence = self.currentPrecedence();

    self.nextToken();

    infix.right = try self.parseExpression(precedence);

    return .{ .infix = infix };
}
fn printCurrentToken(p: *const Parser) void {
    std.debug.print("current token: {s}\n", .{p.cur_token.literal()});
}

fn parseIf(self: *Parser) !ast.Expression {
    var if_exp: ast.If = .{
        .condition = undefined,
        .consequence = undefined,
    };

    self.nextToken();

    if_exp.condition = try self.parseExpression(.lowest);

    if_exp.consequence = if (self.expectPeek(.@"{"))
        try self.parseBlock()
    else
        try self.parseSingleStmtBlock();

    if (self.expectPeek(.@"else")) {
        if_exp.alternative = if (self.expectPeek(.@"{"))
            try self.parseBlock()
        else
            try self.parseSingleStmtBlock();
    }

    return .{ .@"if" = if_exp };
}

// TODO: allow single expression blocks to optionally ignore braces
fn parseBlock(self: *Parser) anyerror!ast.Block {
    const allocator = self.arena.allocator();

    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();

    // : or {
    const tk = self.cur_token;
    var block: ast.Block = .{
        .token = tk,
        .statements = undefined,
    };

    self.nextToken();

    if (tk.tag == .@":") {
        const stmt = try self.parseStatement();
        try stmts.append(stmt);
        // self.nextToken();
    } else {
        while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {
            const stmt = try self.parseStatement();
            try stmts.append(stmt);
            self.nextToken();
        }

        if (!self.curTokenIs(.@"}")) try self.missing(.@"}");
    }

    const stmts_owner = try stmts.toOwnedSlice();

    block.statements = stmts_owner;

    return block;
}

fn parseSingleStmtBlock(self: *Parser) anyerror!ast.Block {
    const allocator = self.arena.allocator();
    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();
    const tk = self.cur_token;
    var block: ast.Block = .{
        .token = tk,
        .statements = undefined,
    };
    self.nextToken();

    const stmt = try self.parseStatement();
    try stmts.append(stmt);
    const stmts_owner = try stmts.toOwnedSlice();
    block.statements = stmts_owner;
    return block;
}

fn parseFn(self: *Parser) !ast.Statement {
    var func_stmt: ast.FunctionStatement = .{
        .func = undefined,
        .name = undefined,
        .token = self.cur_token,
    };

    if (!self.expectPeek(.identifier)) {
        try self.missing(.identifier);
    }

    func_stmt.name = self.parseIdentifier().identifier;

    func_stmt.func = (try self.parseFunction()).function;

    return .{ .@"fn" = func_stmt };
}

fn parseType(self: *Parser) !ast.Expression {
    var struc_or_enum: ast.Type = .{ .type = self.cur_token.tag };

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    var fields: std.ArrayList(ast.Type.Field) = .init(self.arena.allocator());
    errdefer fields.deinit();

    var decls: std.ArrayList(ast.FunctionStatement) = .init(self.arena.allocator());
    errdefer decls.deinit();

    var names: std.StringHashMap(void) = .init(self.arena.allocator());
    defer names.deinit();

    while (!self.peekTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        self.nextToken();

        if (self.curTokenIs(.identifier)) {
            const name = self.parseIdentifier().identifier;

            if (names.contains(name.value)) {
                try self.errors.append("Duplicated field name {s}\n", .{name.value});
            }

            try names.put(name.value, {});

            var value = &NULL;
            if (self.expectPeek(.@"=")) {
                self.nextToken();
                value = try self.parseExpression(.lowest);
            }

            if (self.peekTokenIs(.@":")) try self.unexpected(.@":");

            if (self.peekTokenIs(.@",") or self.curTokenIs(.@";")) {
                self.nextToken();
            }

            try fields.append(.{ .name = name, .value = value });
            continue;
        }

        if (self.curTokenIs(.@"fn")) {
            const tk = self.cur_token;
            if (!self.expectPeek(.identifier)) {
                try self.missing(.identifier);
            }

            const name = self.parseIdentifier().identifier;

            if (names.contains(name.value)) {
                try self.errors.append("Duplicated field name {s}\n", .{name.value});
            }

            try names.put(name.value, {});

            const func = (try self.parseFunction()).function;

            try decls.append(.{
                .name = name,
                .func = func,
                .token = tk,
            });
            continue;
        }

        // // TODO: BIG FIX
        // if (self.curTokenIs(.comment)) {
        //     try comments.append(self.parseComment(self.cur_token));
        //     continue;
        // }

        try self.errlog("Invalid Struct");
        return .bad;
    }

    self.nextToken();

    if (!self.curTokenIs(.@"}")) try self.missing(.@"}");

    struc_or_enum.fields = try fields.toOwnedSlice();
    struc_or_enum.decl = try decls.toOwnedSlice();
    // struc.comments = try comments.toOwnedSlice();

    return .{ .type = struc_or_enum };
}

fn parseFunction(self: *Parser) !ast.Expression {
    var func: ast.Function = .{
        .parameters = undefined,
        .body = undefined,
    };

    if (!self.expectPeek(.@"(")) try self.missing(.@"(");

    func.parameters = try self.parseFunctionParameters();

    if (!self.expectPeek(.@"{")) {
        try self.missing(.@"{");
    }

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
        .value = self.tokenliteral(),
    };

    try indentifiers.append(ident);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const ident2 = self.parseIdentifier().identifier;

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

    // std.debug.print("{}", .{self.peekTokenIs(.@"(")});

    return .{ .method = method_exp };
}

fn parseInstance(self: *Parser, type_exp: *ast.Expression) !ast.Expression {
    const incall = self.call;
    defer self.call = incall;
    self.call = true;
    return .{
        .instance = .{ .type = type_exp, .fields = try self.parseInstanceArguments() },
    };
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

fn parseTuple(self: *Parser, exp: *ast.Expression) !*ast.Expression {
    const allocator = self.arena.allocator();
    const tuple_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(tuple_exp);
    tuple_exp.* = .{ .tuple = .{ .elements = try self.parseExpList(exp) } };
    return tuple_exp;
}

fn newExp(p: *Parser, allocator: std.mem.Allocator) anyerror!*ast.Expression {
    const left_exp = try allocator.create(ast.Expression);
    errdefer allocator.destroy(left_exp);
    left_exp.* = switch (p.cur_token.tag) {
        .identifier => blk: {
            const ident = p.parseIdentifier();
            if (!p.peekTokenIs(.@"{")) {
                break :blk ident;
            }
            const instance = p.types.get(ident.identifier.value) orelse break :blk ident;
            break :blk try p.parseInstance(instance);
        },
        .@"." => p.parseTag(),
        .integer => try p.parseInteger(),
        .float => try p.parseFloat(),
        .true, .false => p.parseBoolean(),
        .null, .@";" => p.parseNull(),
        .@"[" => try p.parseArray(),
        .@"{" => try p.parseHash(),
        .@"fn" => try p.parseFunction(),
        .@"struct", .@"enum" => try p.parseType(),
        .class => try p.parseClass(),
        .string => p.parseString(),
        .char => p.parseChar(),
        .@"if", .@"else" => try p.parseIf(),
        .@"for" => try p.parseFor(),
        .match => try p.parseMatch(),
        .@"!", .@"-" => try p.parsePrefix(),
        .@"(" => try p.parseGroup(),
        else => b: {
            try p.unexpected(p.cur_token.tag);
            break :b .bad;
        },
    };
    // if (left_exp.* == .type) try p.classes.append(left_exp.type.name.?, left_exp);
    return left_exp;
}

fn parseExpList(p: *Parser, exp1: *ast.Expression) ![]*ast.Expression {
    const allocator = p.arena.allocator();
    var exps: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer exps.deinit();

    try exps.append(exp1);

    while (p.peekTokenIs(.@",")) {
        p.nextToken();
        p.nextToken();

        const left_exp = try p.newExp(allocator);
        try exps.append(left_exp);
    }

    return try exps.toOwnedSlice();
}

fn parseMatchArm(self: *Parser) ![]*ast.Expression {
    const incall = self.call;
    defer self.call = incall;
    self.call = true;

    const allocator = self.arena.allocator();
    var args: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer args.deinit();

    const exp = try self.parseExpression(.lowest);
    try args.append(exp);

    while (self.peekTokenIs(.@",")) {
        self.nextToken();
        self.nextToken();

        const exp2 = try self.parseExpression(.lowest);
        try args.append(exp2);
    }

    if (!self.expectPeek(.@"=>")) try self.missing(.@"=>");

    const args_owned = try args.toOwnedSlice();

    return args_owned;
}

pub fn parseString(self: *Parser) ast.Expression {
    return .{
        .string = .{
            .value = self.tokenliteral(),
        },
    };
}

fn debugCurToken(p: *Parser) void {
    std.debug.print("{s}\n", .{p.tokenliteral()});
}

pub fn parseArray(self: *Parser) !ast.Expression {
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

    const incall = self.call;
    defer self.call = incall;
    self.call = true;
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
    const incall = self.call;
    defer self.call = incall;
    self.call = true;

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

    // BUG match () {}
    if (self.curTokenIs(.@"{")) {
        match.value = &TRUE;
    } else {
        // if (!self.curTokenIs(.@"(")) try self.missing(.@"(");
        self.nextToken();
        match.value = try self.parseExpression(.lowest);
        // if (!self.expectPeek(.@")")) try self.missing(.@")");
        if (!self.expectPeek(.@"{")) try self.missing(.@"{");
    }

    self.nextToken();

    const allocator = self.arena.allocator();
    var arms = std.ArrayList(ast.Match.Arm).init(allocator);
    errdefer arms.deinit();

    while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {

        // the else arm
        if (self.curTokenIs(.@"else")) {
            if (match.else_block != null) try self.errlog("Duplicated Tag");

            if (!self.expectPeek(.@"=>")) try self.missing(.@"=>");

            match.else_block = if (self.expectPeek(.@"{"))
                try self.parseBlock()
            else
                try self.parseSingleStmtBlock();

            self.nextToken();
            continue;
        }

        var arm: ast.Match.Arm = .{
            .condition = try self.parseMatchArm(),
            .block = undefined,
        };

        // if (!self.expectPeek(.@"=>")) try self.missing(.@"=>");

        arm.block = if (self.expectPeek(.@"{"))
            try self.parseBlock()
        else
            try self.parseSingleStmtBlock();

        self.nextToken();
        try arms.append(arm);
    }

    match.arms = try arms.toOwnedSlice();

    return .{ .match = match };
}

pub fn parseRange(self: *Parser, left: *ast.Expression) !ast.Expression {
    var range = ast.Range{
        .start = left,
        .end = &NULL,
        .inc = if (self.curTokenIs(.@"..")) .no else .yes,
    };

    if (self.peekTokenIs(.@")") or self.peekTokenIs(.@"]")) {
        return .{ .range = range };
    }

    self.nextToken();

    range.end = try self.parseExpression(.lowest);

    return .{ .range = range };
}

pub fn parseForRange(self: *Parser, ident: ast.Identifier) !ast.Expression {
    var flr = ast.ForRange{
        .ident = ident.value, // for <ident>[, <idx>] in <range> {
        .body = undefined,
        .iterable = undefined,
    };

    if (self.expectPeek(.@",")) {
        self.nextToken();
        const index = self.parseIdentifier();
        flr.index = index.identifier.value;
    }

    if (!self.expectPeek(.in)) {
        try self.missing(.in);
    }

    self.nextToken();

    flr.iterable = try self.parseExpression(.lowest);
    // if (!self.expectPeek(.@")")) try self.missing(.@")");

    if (self.expectPeek(.@"{")) {
        flr.body = try self.parseBlock();
    } else {
        flr.body = try self.parseSingleStmtBlock();
    }

    return .{ .for_range = flr };
}

pub fn parseForLoopCondition(self: *Parser, cond: *ast.Expression) !ast.Expression {
    var fl = ast.For{
        .condition = cond,
        .consequence = undefined,
    };

    if (self.expectPeek(.@"{")) {
        fl.consequence = try self.parseBlock();
    } else {
        fl.consequence = try self.parseSingleStmtBlock();
    }

    return .{ .@"for" = fl };
}

// /// for true { } or for idx, val in list {}
pub fn parseFor(self: *Parser) !ast.Expression {
    if (self.expectPeek(.@"{")) {
        return .{ .@"for" = .{ .condition = &TRUE, .consequence = try self.parseBlock() } };
    }

    // if (!self.expectPeek(.@"(")) try self.missing(.@"(");
    self.nextToken();

    const incall = self.call;
    self.call = true;
    const condition_or_ident = try self.parseExpression(.lowest);
    self.call = incall;

    if (condition_or_ident.* == .identifier) {
        if (self.peekTokenIs(.in) or self.peekTokenIs(.@",")) {
            return self.parseForRange(condition_or_ident.identifier);
        }
    }

    // if (!self.expectPeek(.@")")) try self.missing(.@")");

    return self.parseForLoopCondition(condition_or_ident);
}

/// TODO: what about pear type resolution?
fn missing(p: *Parser, tk: Token.Tag) !void {
    const l = p.line();
    const lin = l.line;
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        "\x1b[1m{s}:{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m Expected '{s}', found '{s}' ({s}) \x1b[0m\n\t{s}\n\t",
        .{ p.errors.file, p.lexer.line_index, at, @tagName(tk), p.peektokenliteral(), @tagName(p.peek_token.tag), lin },
    );
    try p.errors.msg.writer().writeByteNTimes(' ', at - 1);
    try p.errors.msg.writer().writeAll("\x1b[32m^\x1b[0m\n");
}

fn unexpected(p: *Parser, tk: Token.Tag) !void {
    const l = p.line();
    const lin = l.line;
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        "\x1b[1m{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m Unexpected token '{s}'\x1b[0m\n\t{s}\n\t",
        .{ p.lexer.line_index, at, @tagName(tk), lin },
    );
    try p.errors.msg.writer().writeByteNTimes(' ', at - 1);
    try p.errors.msg.writer().writeAll("\x1b[32m^\x1b[0m\n");
}

fn errlog(p: *Parser, msg: []const u8) !void {
    const l = p.line();
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
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
            // .@"(", .@"{" => .call,
            .@"(" => .call,
            .@"!" => .prefix,
            .@"[", .@".", .@"..", .@"..=" => .index,
            .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => .assigne,
            // .@"," => .tuple,
        };
    }
};

fn parseClass(self: *Parser) !ast.Expression {
    var class: ast.Class = .{ .type = self.cur_token.tag, .name = "" };

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    var fields: std.StringArrayHashMap(ast.Class.Field) = .init(self.arena.allocator());
    errdefer fields.deinit();

    var decls: std.StringArrayHashMap(ast.FunctionStatement) = .init(self.arena.allocator());
    errdefer decls.deinit();

    while (!self.peekTokenIs(.@"}") and !self.curTokenIs(.eof)) {
        if (self.peekTokenIs(.@"fn")) break;
        self.nextToken();

        if (!self.curTokenIs(.identifier)) {
            try self.errlog("Invalid Class syntax");
            return .bad;
        }

        const name = self.parseIdentifier().identifier.value;
        if (fields.contains(name)) {
            try self.errors.append("Duplicated field name {s}\n", .{name});
        }

        var value = &NULL;
        if (self.expectPeek(.@"=")) {
            self.nextToken();
            value = try self.parseExpression(.lowest);
        }

        try fields.put(name, .{ .name = name, .value = value });
    }

    self.nextToken();

    while (self.curTokenIs(.@"fn")) {
        const fn_stmt = try self.parseFn();
        const func = fn_stmt.@"fn";
        const name = func.name.value;
        if (decls.contains(name)) {
            try self.errors.append("Duplicated method name {s}\n", .{name});
        }
        try decls.put(name, func);
        self.nextToken();
    }

    if (!self.curTokenIs(.@"}")) try self.missing(.@"}");
    if (self.curTokenIs(.eof)) try self.errlog("EOF");

    class.fields = fields.values();
    class.decls = decls.values();

    return .{ .class = class };
}
