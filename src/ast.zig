const std = @import("std");
const Token = @import("Token.zig");

// interface
pub const Node = union(enum) {
    expression: *Expression,
    statement: Statement,

    fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    pub fn position(self: *const Node) usize {
        switch (self.*) {
            inline else => |node| return node.position(),
        }
    }

    pub fn commentPos(self: *const Node) ?[2]usize {
        switch (self.*) {
            inline else => |node| return node.commentPos(),
        }
    }

    pub fn deinit(self: *Node, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .statement => |stmt| switch (stmt) {
                .block => |blk| alloc.free(blk.statements),
                .program => |p| p.statements.deinit(),
                else => {},
            },
            else => {},
        }
    }
};

/// implements Node,
/// interface: {return, var, expression} statements
pub const Statement = union(enum) {
    // implements Node,
    @"var": Var,
    var_block: VarBlock,

    con: Con,
    // con_block: ConBlock,

    @"return": Return,
    @"break": Break,
    @"continue": Continue,

    @"defer": Defer,

    block: Block,

    @"fn": FunctionStatement,
    exp_statement: ExpStatement,

    program: Program,

    // comment: Comment,

    fn statementNode(self: *const Statement) void {
        switch (self.*) {
            inline else => |x| x.statementNode(),
        }
    }

    // interface methods
    fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn position(self: *const Statement) usize {
        switch (self.*) {
            inline else => |s| return s.at,
        }
    }

    pub fn commentPos(self: *const Statement) ?[2]usize {
        return switch (self.*) {
            inline .@"var", .con, .@"fn", .@"return", .exp_statement => |v| v.token.comment_pos,
            inline else => null,
        };
    }
};

pub const TypeLiteral = union(enum) {
    Identifier,
    BuiltinType,
};

/// implements Node,
pub const Expression = union(enum) {
    null,
    bad,
    // type: TypeLiteral,
    boolean: Boolean,
    float: Float,
    integer: Integer,
    char: Char,
    string: String,
    array: Array,
    range: Range,
    hash: Hash,
    // enum_literal: EnumLiteral,
    tag: Tag,
    identifier: Identifier,
    // forloop_expression: ForLoopExpression,
    for_range: ForRange,
    // multi_forloop_range_expression: MultiForLoopRangeExpression,
    @"for": For,
    @"if": If,
    match: Match,
    function: Function,

    prefix: Prefix,
    infix: Infix,
    assignment: Assignment,

    call: Call,
    instance: Instance,

    method: Method,
    index: Index,

    type: Type,
    group: Group,

    fn expressionNode(self: *const Expression) void {
        _ = self;
    }

    // interface methods
    fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |exp| exp.tokenLiteral(),
        };
    }

    pub fn position(self: *const Expression) usize {
        switch (self.*) {
            .null, .bad => return 0,
            inline else => |s| return s.at,
        }
    }

    pub fn commentPos(self: *const Expression) ?[2]usize {
        return switch (self.*) {
            inline else => null,
        };
    }
};

pub const Comment = struct {
    at: usize,
    end: usize,
};

pub const Group = struct {
    at: usize = 0,
    exp: *Expression,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "(";
    }
};

pub const If = struct {
    at: usize = 0,
    condition: *Expression,
    consequence: Block,
    alternative: ?Block = null,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "if";
    }
};

pub const Prefix = struct {
    at: usize = 0,
    operator: Token.Type,
    right: *Expression,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return @tagName(self.operator);
    }
};

pub const Infix = struct {
    at: usize = 0,
    left: *Expression,
    operator: Token.Type,
    right: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return @tagName(self.operator);
    }
};

pub const Index = struct {
    at: usize = 0,
    // [
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "[";
    }
};

//--------------------------------------------

pub const Identifier = struct {
    at: usize = 0,
    //= .identifier,
    value: []const u8,

    pub fn expressionNode(self: *const @This()) void {
        _ = self;
    }
    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "identifier";
    }
};

pub const Program = struct {
    at: usize = 0,
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return if (self.statements.items.len > 0) self.statements.items[0].tokenLiteral() else "";
    }
};

// ------------------------------------------------------------------------
pub const Con = struct {
    token: Token,
    at: usize = 0,
    //= .@"var",
    name: []Identifier,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "con";
    }
};

pub const VarBlock = struct {
    at: usize = 0,
    //= .@"var",
    vars_decl: []Var,
};

pub const ConBlock = struct {
    at: usize = 0,
    //= .@"var",
    const_decl: []Con,
};

pub const Var = struct {
    token: Token,
    at: usize = 0,
    name: Identifier,
    value: *Expression,
    // type: ?*Expression = null,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "var";
    }
};

pub const Return = struct {
    token: Token,
    at: usize = 0,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "return";
    }
};

pub const Continue = struct {
    at: usize = 0,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "continue";
    }
};

pub const Break = struct {
    at: usize = 0,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "break";
    }
};

// identifier = expression
//  = , +=, -=, *=, /=,
pub const Assignment = struct {
    at: usize = 0,
    name: *Expression,
    operator: Token.Type,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return @tagName(self.operator);
    }
};

pub const ExpStatement = struct {
    token: Token,
    at: usize = 0,
    // fist token only
    expression: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Block = struct {
    token: Token,
    at: usize = 0,
    statements: []Statement,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "{";
    }
};
// --------------------------------------------------------------------

pub const Float = struct {
    at: usize = 0,
    value: f32,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Integer = struct {
    at: usize = 0,
    value: i32,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        var buf: [10]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{}", .{self.value}) catch "";
    }
};

pub const Char = struct {
    at: usize = 0,
    value: u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.value;
    }
};

pub const String = struct {
    at: usize = 0,
    value: []const u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.value;
    }
};

pub const Boolean = struct {
    at: usize = 0,
    value: bool,

    pub fn typeIs() BuiltinType {
        return .boolean;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return if (self.value) "true" else "false";
    }
};

pub const Array = struct {
    at: usize = 0,
    elements: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Range = struct {
    at: usize = 0,
    start: *Expression,
    end: *Expression,
    inc: enum { yes, no } = .no,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "..";
    }
};

// pub const Hash = struct { at: usize = 0,
//     pairs: std.AutoHashMap(*Expression, *Expression),
//
//     pub fn tokenLiteral(self: *const @This()) []const u8 {
//         return self.token.literal;
//     }
// };

pub const Hash = struct {
    at: usize = 0,
    pairs: [][2]*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const EnumLiteral = struct {
    at: usize = 0,
    tags: std.StringHashMap(*Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Tag = struct {
    at: usize = 0,
    value: []const u8,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionStatement = struct {
    token: Token,
    at: usize = 0,
    name: Identifier,
    func: Function,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Defer = struct {
    at: usize = 0,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Function = struct {
    at: usize = 0,
    name: ?Identifier = null,
    parameters: []Identifier,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Call = struct {
    at: usize = 0,
    // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Type = struct {
    at: usize = 0,
    type: Token.Type,
    name: ?[]const u8 = null,
    fields: []Field = &.{},
    desc: []FunctionStatement = &.{},
    // comments: []Statement = &.{},

    pub const Field = struct {
        at: usize = 0,
        name: Identifier,
        value: *Expression,
    };

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Instance = struct {
    at: usize = 0,
    type: *Expression, // Identifier or FunctionLiteral
    fields: []Type.Field,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Method = struct {
    at: usize = 0,
    // .
    caller: *Expression, //
    method: Identifier,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};
const ForLoopMode = enum { infinity, decl_infinity, decl_cond, dec_cond_inc, cond, cond_inc, range, multi_range };
// for i = 0, i < 10, i++, i in 0..10
pub const For = struct {
    at: usize = 0,
    //TODO: list of conditions
    condition: *Expression,
    consequence: Block,
    mode: ForLoopMode = .infinity,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const MultiForLoopRange = struct {
    pub const LoopVars = struct {
        at: usize = 0,
        ident: []const u8,
        index: ?[]const u8 = null,
        iterable: *Expression,
    };

    at: usize = 0,
    loops: []LoopVars,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ForRange = struct {
    at: usize = 0,
    ident: []const u8,
    index: ?[]const u8 = null,
    iterable: *Expression,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Match = struct {
    at: usize = 0,
    value: *Expression,
    arms: []Arm,
    else_block: ?Block = null,

    pub const Arm = struct {
        at: usize = 0,
        // =>
        condition: *Expression,
        block: Block,
    };

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

const BuiltinType = union(enum) {
    bool,

    int,
    uint,
    float,

    null,

    string,

    func: struct {
        at: usize = 0,
        args_type: []const TypeLiteral,
        return_type: TypeLiteral,
    },
};
