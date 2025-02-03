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
context_parser: bool = false,
/// TODO: what about pear type resolution?
types: std.StringHashMap(void),

fn missing(p: *Parser, tk: Token.Type) !void {
    const l = p.line();
    const lin = l.line;
    const at = p.lexer.position - l.start - 1;
    try p.errors.append(
        "\x1b[1m{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m Expected '{s}', found '{s}' ({s}) \x1b[0m\n\t{s}\n\t",
        .{ p.lexer.line_index, at, @tagName(tk), p.peek_token.literal, @tagName(p.peek_token.type), lin },
    );
    try p.errors.msg.writer().writeByteNTimes(' ', at - 1);
    try p.errors.msg.writer().writeAll("\x1b[32m^\x1b[0m\n");
}

fn unexpected(p: *Parser, tk: Token.Type) !void {
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
        "\x1b[1m{}:{}: \x1b[31mSyntax error:\x1b[0m\x1b[1m {s} \x1b[0m\n",
        .{ p.lexer.line_index, at, msg },
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
var ZERO_INT: ast.Expression = .{ .integer = .{ .value = 0 } };
var ZERO_FLOAT: ast.Expression = .{ .float = .{ .value = 0 } };
var ZERO_CHAR: ast.Expression = .{ .char = .{ .value = 0 } };
var ZERO_STRING: ast.Expression = .{ .string = .{ .value = "" } };

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
    instance,
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
            .@"{" => .instance,
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

    const tk = p.cur_token;
    left_exp.* = switch (tk.type) {
        // dont allow Initialization {}
        .@"." => p.parseTag(),
        .integer => try p.parseInteger(),
        .float => try p.parseFloat(),
        .true, .false => p.parseBoolean(),
        .null, .@";" => p.parseNull(),
        .string => p.parseString(),
        .char => p.parseChar(),
        .@"[" => try p.parseArray(),
        .@"{" => try p.parseHash(),
        .@"!", .@"-" => try p.parsePrefix(),
        // allow initialization {}
        .@"fn" => try p.parseFunction(),
        .identifier => p.parseIdentifier(),
        .@"struct", .@"enum" => try p.parseType(),
        .@"if", .@"else" => try p.parseIf(),
        .@"for" => try p.parseFor(),
        .match => try p.parseMatch(),
        .@"(" => try p.parseGroup(),
        // .comment => {
        //     p.nextToken();
        //     return p.prefixExp();
        // },
        else => b: {
            try p.unexpected(p.cur_token.type);
            break :b .bad;
        },
    };

    // if (p.curTokenIs(.identifier) or p.curTokenIs(.@"struct")) l: {
    //     if (!p.peekTokenIs(.@"{")) break :l;
    //
    //     if (left_exp.* != .identifier) break :l;
    //     if (p.context_parser) break :l;
    //     if (!p.types.contains(left_exp.identifier.value)) {
    //         try p.errlog("literal is not a type");
    //         break :l;
    //     }
    //
    //     const left_exp2 = try allocator.create(ast.Expression);
    //     errdefer allocator.destroy(left_exp2);
    //     left_exp2.* = try p.parseInstance(left_exp);
    //     return left_exp2;
    // }

    // // TODO: better logic <<EXPERIMENTAL>>
    // if (p.peekTokenIs(.@",") and t) {
    //     const left_exp2 = try allocator.create(ast.Expression);
    //     errdefer allocator.destroy(left_exp2);
    //     left_exp2.* = try p.parseTuple(left_exp);
    //
    //     switch (left_exp2.*) {
    //         .bad, .null => {},
    //         inline else => |*exp| exp.at = tk.at,
    //     }
    //
    //     return left_exp2;
    // }

    switch (left_exp.*) {
        .bad, .null => {},
        inline else => |*exp| exp.at = tk.at,
    }

    return left_exp;
}

fn infixExp(p: *Parser, lx: *ast.Expression) anyerror!?ast.Expression {
    var tk: Token = p.peek_token;
    var left_exp: ?ast.Expression = b: switch (tk.type) {
        // allw initilization
        .@"[" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseIndex(lx);
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
        // dont allow
        .@"(" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseCall(lx);
        },
        .@"..", .@"..=" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseRange(lx);
        },
        .@"%", .@"+", .@"-", .@"==", .@"!=", .@"*", .@"or", .@"and", .@"/", .@">", .@">=" => {
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
                .at = tk.at,
            } };
        },
        .@"=", .@":=", .@"+=", .@"-=", .@"*=", .@"/=" => {
            p.nextToken();
            tk = p.cur_token;
            break :b try p.parseAssignment(lx);
        },

        else => break :b null,
    };

    if (left_exp) |*lf| switch (lf.*) {
        .bad, .null => {},
        inline else => |*exp| exp.at = tk.at,
    };

    return left_exp;
}

pub fn init(child_alloc: std.mem.Allocator, lexer: *Lexer, errors: *Error) Parser {
    const arena: std.heap.ArenaAllocator = .init(child_alloc);
    // const allocator = arena.allocator();

    var parser: Parser = .{
        .arena = arena,
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .last_token = .{ .type = .eof, .literal = "" },
        .errors = errors,
        .types = .init(child_alloc),
    };

    parser.nextToken();
    parser.nextToken();

    return parser;
}

pub fn deinit(self: *Parser) void {
    self.types.deinit();
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
    return .{ .identifier = .{ .value = self.cur_token.literal, .at = self.lexer.position } };
}

fn parseTag(self: *Parser) ast.Expression {
    self.nextToken();
    return .{ .tag = .{ .value = self.cur_token.literal } };
}

fn parseReturn(self: *Parser) !ast.Statement {
    var return_stmt: ast.Return = .{ .value = &NULL, .token = self.cur_token };

    if (self.peekTokenIs(.@"}")) {
        return .{ .@"return" = return_stmt };
    }

    self.nextToken();

    return_stmt.value = try self.parseExpression(.lowest);

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

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

    const path_exp = try self.parseExpression(.lowest);

    if (path_exp.* != .string) {
        @panic("import: invalid expression; expected string type");
    }

    const file_path = path_exp.string.value;
    const name = try self.findImportName(file_path);
    const imput = std.fs.cwd().readFileAlloc(self.arena.allocator(), file_path, 4 * 1024) catch |err| b: {
        try self.errlog(@errorName(err));
        break :b "";
    };
    var new_lexer = Lexer.init(imput);

    // old state
    const last_lexer = self.lexer;
    const last_cur_token = self.cur_token;
    const last_peek_token = self.peek_token;
    const last_last_token = self.last_token;

    // new state
    self.lexer = &new_lexer;
    self.cur_token = undefined;
    self.peek_token = undefined;
    self.last_token = .{ .type = .eof, .literal = "" };

    self.nextToken();
    self.nextToken();

    const node = try self.arena.allocator().create(ast.Node);
    node.* = try self.parse();

    // back to old state
    self.lexer = last_lexer;
    self.cur_token = last_cur_token;
    self.peek_token = last_peek_token;
    self.last_token = last_last_token;

    return .{
        .import = .{
            .name = .{ .value = name, .at = tk.at },
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

    stmt.operator = switch (op.type) {
        .@"=", .@":=", .@"+=", .@"-=", .@"/=", .@"*=" => op.type,
        else => b: {
            try self.unexpected(op.type);
            try self.errlog("Invalid Operator");
            break :b op.type;
        },
    };

    // for now, this will work
    if (self.curTokenIs(.@"struct") and !self.peekTokenIs(.@"{")) {
        if (name.* == .identifier) {
            if (std.ascii.isLower(name.identifier.value[0])) try self.errlog("Type values must start with UpperCase");
            try self.types.put(name.identifier.value, {});
        }
    }

    stmt.value = try self.parseExpression(.lowest);

    if ((op.type == .@":=" or op.type == .@"=") and stmt.value.* == .type) {
        if (name.* == .identifier) {
            if (std.ascii.isLower(name.identifier.value[0])) try self.errlog("Type values must start with UpperCase");
            try self.types.put(stmt.name.identifier.value, {});
        }
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

    while (self.cur_token.type != .@")") {
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
        .value = self.cur_token.literal,
        .at = self.lexer.position,
    };

    if (!self.expectPeek(.@"=")) {
        self.nextToken();
        // const var_type = self.parseIdentifier().identifier;
        // var_stmt.type = var_type;

        if (!self.expectPeek(.@"=")) {
            // if (std.mem.eql(u8, var_type.value, "int")) {
            //     var_stmt.value = &ZERO_INT;
            // } else if (std.mem.eql(u8, var_type.value, "float")) {
            //     var_stmt.value = &ZERO_FLOAT;
            // } else if (std.mem.eql(u8, var_type.value, "bool")) {
            //     var_stmt.value = &TRUE;
            // } else if (std.mem.eql(u8, var_type.value, "string")) {
            //     var_stmt.value = &ZERO_STRING;
            // } else if (std.mem.eql(u8, var_type.value, "null")) {
            //     var_stmt.value = &NULL;
            // } else {
            //     var_stmt.value = &NULL;
            // }
            // var_stmt.value = &NULL;

            if (self.peekTokenIs(.@";")) {
                self.nextToken();
            }

            return .{ .@"var" = var_stmt };
        }
    }

    self.nextToken();

    const name = var_stmt.name.value;
    // for now, this will work
    if (self.curTokenIs(.@"struct")) {
        if (std.ascii.isLower(name[0])) try self.errlog("Type values must start with UpperCase");
        try self.types.put(name, {});
    }

    var_stmt.value = try self.parseExpression(.lowest);

    if (var_stmt.value.* == .type) {
        if (std.ascii.isLower(name[0])) try self.errlog("Type values must start with UpperCase.");
        try self.types.put(name, {});
    } else {
        _ = self.types.remove(name);
    }

    if (self.peekTokenIs(.@";")) {
        self.nextToken();
    }

    return .{ .@"var" = var_stmt };
}

fn parseStatement(self: *Parser) !ast.Statement {
    const tk = self.cur_token;
    var stmt: ast.Statement = switch (tk.type) {
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

    switch (stmt) {
        inline else => |*st| st.at = tk.at,
    }

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

fn instantiatable(self: *Parser, exp: *ast.Expression) bool {
    return switch (exp.*) {
        .identifier => |ident| l: {
            const x = self.types.contains(ident.value);
            break :l x;
        },
        .method => |method| l: {
            const ident = method.method;
            const x = self.types.contains(ident.value);
            break :l x;
        },

        // .for_range => |fr| self.instantiatable(fr.iterable),
        .call => |call| self.instantiatable(call.function),

        .function => |func| l: {
            const ident = func.name orelse return false;
            const x = self.types.contains(ident.value);
            break :l x;
        },

        .group => |gang| l: {
            break :l self.instantiatable(gang.exp);
        },

        // .type => |ty| l: {
        //     ty.
        //     break :l self.instantiatable(gang.exp);
        // },
        .type, .@"if", .match, .@"for", .for_range, .index => true,

        else => false,
    };
}

fn parseExpression(self: *Parser, precedence: Precedence) !*ast.Expression {
    const left_exp = try self.prefixExp();
    const allocator = self.arena.allocator();

    while (@intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
        if (self.peekTokenIs(.@"{") and !self.instantiatable(left_exp)) break;

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
    self.nextToken();

    self.context_parser = false;
    const exp = try self.parseExpression(.lowest);

    if (!self.expectPeek(.@")")) try self.missing(.@")");

    return .{ .group = .{ .exp = exp } };
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
        .at = self.lexer.position,
    };

    const precedence = self.currentPrecedence();

    self.nextToken();

    infix.right = try self.parseExpression(precedence);

    return .{ .infix = infix };
}

fn parseIf(self: *Parser) !ast.Expression {
    var if_exp: ast.If = .{
        .condition = undefined,
        .consequence = undefined,
    };

    self.nextToken();

    self.context_parser = true;
    if_exp.condition = try self.parseExpression(.lowest);
    self.context_parser = false;

    if (!self.expectPeek(.@"{") and !self.expectPeek(.@":")) {
        try self.missing(.@"{");
    }

    if_exp.consequence = try self.parseBlock();

    if (self.peekTokenIs(.@"else")) {
        self.nextToken();
        if (!self.expectPeek(.@"{")) try self.missing(.@"{");
        if_exp.alternative = try self.parseBlock();
    }

    return .{ .@"if" = if_exp };
}

// TODO: allow single expression blocks to optionally ignore braces
fn parseBlock(self: *Parser) anyerror!ast.Block {
    const allocator = self.arena.allocator();
    var mask = false;

    var stmts: std.ArrayList(ast.Statement) = .init(allocator);
    errdefer stmts.deinit();

    // : or {
    const tk = self.cur_token;
    var block: ast.Block = .{
        .token = tk,
        .statements = undefined,
    };

    self.nextToken();

    if (tk.type == .@":") {
        const stmt = try self.parseStatement();
        if (stmt == .@"return") if (stmt.@"return".value.* == .type) {
            mask = true;
        };
        try stmts.append(stmt);
        // self.nextToken();
    } else {
        while (!self.curTokenIs(.@"}") and !self.curTokenIs(.eof)) {
            const stmt = try self.parseStatement();
            if (stmt == .@"return") if (stmt.@"return".value.* == .type) {
                mask = true;
            };
            try stmts.append(stmt);
            self.nextToken();
        }

        if (!self.curTokenIs(.@"}")) try self.missing(.@"}");
    }

    const stmts_owner = try stmts.toOwnedSlice();

    block.statements = stmts_owner;
    block.mask = mask;

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

    // TODO: emmit a parser/compilation erros if a function
    // with no uppercase name returns a type
    if (std.ascii.isUpper(func_stmt.name.value[0])) {
        try self.types.put(func_stmt.name.value, {});
    }

    func_stmt.func = (try self.parseFunction()).function;

    if (func_stmt.func.mask and std.ascii.isLower(func_stmt.name.value[0])) {
        try self.errlog("Function that return 'types' must have a name with at least the first letter capitalized");
    } else if (!func_stmt.func.mask and std.ascii.isUpper(func_stmt.name.value[0])) {
        try self.errlog("invalid function name: fuck you");
    }

    return .{ .@"fn" = func_stmt };
}

fn parseType(self: *Parser) !ast.Expression {
    var struc: ast.Type = .{ .type = self.cur_token.type };

    if (!self.expectPeek(.@"{")) try self.missing(.@"{");

    var fields: std.ArrayList(ast.Type.Field) = .init(self.arena.allocator());
    errdefer fields.deinit();

    var descs: std.ArrayList(ast.FunctionStatement) = .init(self.arena.allocator());
    errdefer descs.deinit();

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
                if (value.* == .type) {
                    try self.errlog("A field with value 'Type' is not allowed. sorry");
                }
            }

            if (self.peekTokenIs(.@":")) try self.unexpected(.@":");
            if (self.peekTokenIs(.@",") or self.curTokenIs(.@";")) {
                self.nextToken();
            }

            try fields.append(.{
                .name = name,
                .value = value,
            });
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
            if (func.mask and std.ascii.isLower(name.value[0])) {
                try self.errlog("Function that return 'types' must have a name with at least the first letter capitalized");
            } else if (!func.mask and std.ascii.isUpper(name.value[0])) {
                try self.errlog("Invalid function name; tip: follow the fucking naming convention");
            }

            try descs.append(.{
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

    struc.fields = try fields.toOwnedSlice();
    struc.desc = try descs.toOwnedSlice();
    // struc.comments = try comments.toOwnedSlice();

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
    func.mask = func.body.mask;

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
        .at = self.lexer.position,
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
    return .{
        .instance = .{ .type = type_exp, .fields = try self.parseInstanceArguments() },
    };
}

fn parseInstance3(self: *Parser) !ast.Expression {
    return .{ .instance = .{
        .type = try self.parseExpression(.lowest),
        .fields = try self.parseInstanceArguments(),
    } };
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

fn debugCurTokenIs(p: *Parser, tk: Token.Type) void {
    std.debug.print("{} {}\n", .{ p.curTokenIs(tk), p.context_parser });
}

fn debugCurToken(p: *Parser) void {
    std.debug.print("{} {} {}\n", .{ p.cur_token.type, p.peek_token.type, p.context_parser });
}

var t = true;
pub fn parseTuple(self: *Parser, exp: *ast.Expression) !ast.Expression {
    t = false;
    const allocator = self.arena.allocator();
    var elements: std.ArrayList(*ast.Expression) = .init(allocator);
    errdefer elements.deinit();

    try elements.append(exp);
    while (self.expectPeek(.@",")) {
        self.nextToken(); // next_element
        const element_n = try self.parseExpression(.lowest);
        try elements.append(element_n);
    }

    t = true;
    return .{
        .array = .{ .elements = try elements.toOwnedSlice() },
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
        if (val.* == .type) {
            try self.errlog("A hash field is not allowed to be a type");
        }

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
        self.context_parser = true;
        match.value = try self.parseExpression(.lowest);
        self.context_parser = false;
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

    if (self.expectPeek(.@",")) {
        const index = self.parseIdentifier();
        flr.index = index.identifier.value;
        self.nextToken();
    }
    //
    if (!self.expectPeek(.in)) {
        try self.missing(.in);
    }

    self.nextToken();

    self.context_parser = true;
    flr.iterable = try self.parseExpression(.lowest);
    self.context_parser = false;

    // if (self.expectPeek(.@";") and !self.peekTokenIs(.@"{")) {
    //     return self.parseMultiForLoopRange(flr);
    // }

    if (!self.expectPeek(.@"{") and !self.expectPeek(.@":")) {
        try self.missing(.@"{");
    }

    flr.body = try self.parseBlock();

    return .{ .for_range = flr };
}

pub fn parseForLoopCondition(self: *Parser, cond: *ast.Expression) !ast.Expression {
    var fl = ast.For{
        .condition = cond,
        .consequence = undefined,
    };

    if (!self.expectPeek(.@"{") and !self.expectPeek(.@":")) {
        try self.missing(.@"{");
    }

    fl.consequence = try self.parseBlock();

    return .{ .@"for" = fl };
}

// /// for true { } or for idx, val in list {}
pub fn parseFor(self: *Parser) !ast.Expression {
    if (self.expectPeek(.@"{")) {
        return .{ .@"for" = .{ .condition = &TRUE, .consequence = try self.parseBlock() } };
    }

    self.nextToken();

    self.context_parser = true;
    const condition_or_ident = try self.parseExpression(.lowest);
    self.context_parser = false;

    if (condition_or_ident.* == .identifier) {
        if (self.peekTokenIs(.in) or self.peekTokenIs(.@",")) {
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
