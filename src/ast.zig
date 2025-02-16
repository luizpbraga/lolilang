const std = @import("std");
const Token = @import("Token.zig");

// interface
pub const Node = union(enum) {
    expression: *Expression,
    statement: Statement,

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

    import: Import,
    @"pub": Pub,
    // comment: Comment,

    fn statementNode(self: *const Statement) void {
        switch (self.*) {
            inline else => |x| x.statementNode(),
        }
    }

    pub fn position(self: *const Statement) usize {
        switch (self.*) {
            inline else => return 0,
        }
    }

    pub fn commentPos(self: *const Statement) ?[2]usize {
        return switch (self.*) {
            // inline .@"var", .con, .@"fn", .@"return", .exp_statement => |v| v.token.comment_pos,
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
    tuple: Tuple,
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

    pub fn position(self: *const Expression) usize {
        switch (self.*) {
            .null, .bad => return 0,
            inline else => return 0,
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
    exp: *Expression,
};

pub const If = struct {
    condition: *Expression,
    consequence: Block,
    alternative: ?Block = null,
};

pub const Prefix = struct {
    operator: Token.Tag,
    right: *Expression,
};

pub const Infix = struct {
    left: *Expression,
    operator: Token.Tag,
    right: *Expression,
};

pub const Index = struct {
    // [
    left: *Expression,
    index: *Expression,
};

//--------------------------------------------

pub const Identifier = struct {
    //= .identifier,
    value: []const u8,
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
};

// ------------------------------------------------------------------------
pub const Con = struct {
    token: Token,
    //= .@"var",
    name: []Identifier,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const VarBlock = struct {
    //= .@"var",
    vars_decl: []Var,
};

pub const ConBlock = struct {
    //= .@"var",
    const_decl: []Con,
};

pub const Var = struct {
    token: Token,
    name: Identifier,
    value: *Expression,
    // type: ?*Expression = null,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Import = struct {
    token: Token,
    name: Identifier,
    path: *Expression,
    node: *Node,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Pub = struct {
    token: Token,
    stmt: *Statement,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Return = struct {
    token: Token,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Continue = struct {
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Break = struct {
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

// identifier = expression
//  = , +=, -=, *=, /=,
pub const Assignment = struct {
    name: *Expression,
    operator: Token.Tag,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const ExpStatement = struct {
    token: Token,
    // fist token only
    expression: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }
};

pub const Block = struct {
    token: Token,
    statements: []Statement,
};
// --------------------------------------------------------------------

pub const Float = struct {
    value: f32,
};

pub const Integer = struct {
    value: i32,
};

pub const Char = struct {
    value: u8,
};

pub const String = struct {
    value: []const u8,
};

pub const Boolean = struct {
    value: bool,
};

pub const Tuple = struct {
    elements: []*Expression,
};

pub const Array = struct {
    elements: []*Expression,
};

pub const Range = struct {
    start: *Expression,
    end: *Expression,
    inc: enum { yes, no } = .no,
};

pub const Hash = struct {
    pairs: [][2]*Expression,
};

pub const EnumLiteral = struct {
    tags: std.StringHashMap(*Expression),
};

pub const Tag = struct {
    value: []const u8,
};

pub const FunctionStatement = struct {
    token: Token,
    name: Identifier,
    func: Function,
};

pub const Defer = struct {
    body: Block,
};

pub const Function = struct {
    name: ?Identifier = null,
    parameters: []Identifier,
    body: Block,
};

pub const Call = struct {
    // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []*Expression,
};

pub const Type = struct {
    type: Token.Tag,
    name: ?[]const u8 = null,
    fields: []Field = &.{},
    decl: []FunctionStatement = &.{},
    // comments: []Statement = &.{},

    pub const Field = struct {
        name: Identifier,
        value: *Expression,
    };
};

pub const Instance = struct {
    type: *Expression, // Identifier or FunctionLiteral
    fields: []Type.Field,
};

pub const Method = struct {
    // .
    caller: *Expression, //
    method: Identifier,
};
const ForLoopMode = enum { infinity, decl_infinity, decl_cond, dec_cond_inc, cond, cond_inc, range, multi_range };
// for i = 0, i < 10, i++, i in 0..10
pub const For = struct {
    //TODO: list of conditions
    condition: *Expression,
    consequence: Block,
    mode: ForLoopMode = .infinity,
};

pub const MultiForLoopRange = struct {
    pub const LoopVars = struct {
        ident: []const u8,
        index: ?[]const u8 = null,
        iterable: *Expression,
    };

    loops: []LoopVars,
    body: Block,
};

pub const ForRange = struct {
    ident: []const u8,
    index: ?[]const u8 = null,
    iterable: *Expression,
    body: Block,
};

pub const Match = struct {
    value: *Expression,
    arms: []Arm,
    else_block: ?Block = null,

    pub const Arm = struct {
        // =>
        condition: []*Expression,
        block: Block,
    };
};
