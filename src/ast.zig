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
};

/// implements Node,
/// interface: {return, var, expression} statements
pub const Statement = union(enum) {
    // implements Node,
    @"var": Var,
    var_block: VarBlock,

    con: Con,
    con_block: ConBlock,

    @"return": Return,

    @"break": Break,
    @"defer": Defer,

    block: Block,

    // function: FunctionStatement,
    exp_statement: ExpStatement,

    program: Program,

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
};

pub const TypeLiteral = union(enum) {
    Identifier,
    BuiltinType,
};

/// implements Node,
pub const Expression = union(enum) {
    null,
    // type: TypeLiteral,
    boolean: Boolean,
    float: Float,
    integer: Integer,
    string: String,
    array: Array,
    range: Range,
    hash: Hash,
    // enum_literal: EnumLiteral,
    // enum_tag: EnumTag,
    identifier: Identifier,
    // forloop_expression: ForLoopExpression,
    // forloop_range_expression: ForLoopRangeExpression,
    // multi_forloop_range_expression: MultiForLoopRangeExpression,
    @"if": If,
    match: Match,
    function: Function,

    prefix: Prefix,
    infix: Infix,
    assignment: Assignment,

    call: Call,
    method: Method,
    index: Index,

    fn expressionNode(self: *const Expression) void {
        _ = self;
    }

    // interface methods
    fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |exp| exp.tokenLiteral(),
        };
    }
};

pub const If = struct {
    token: Token,
    condition: *Expression,
    consequence: Block,
    alternative: ?Block = null,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Prefix = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Infix = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Index = struct {
    token: Token, // [
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

//--------------------------------------------

pub const Identifier = struct {
    token: Token, //= .identifier,
    value: []const u8,

    pub fn expressionNode(self: *const @This()) void {
        _ = self;
    }
    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return if (self.statements.items.len > 0) self.statements.items[0].tokenLiteral() else "";
    }
};

// ------------------------------------------------------------------------
pub const Con = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const VarBlock = struct {
    token: Token, //= .@"var",
    vars_decl: []Var,
};

pub const ConBlock = struct {
    token: Token, //= .@"var",
    const_decl: []Con,
};

pub const Var = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: *Expression,
    type: ?*Expression = null,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Return = struct {
    token: Token,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Break = struct {
    token: Token,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

// identifier = expression
//  = , +=, -=, *=, /=,
pub const Assignment = struct {
    token: Token,
    name: *Expression,
    operator: []const u8,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ExpStatement = struct {
    token: Token, // fist token only
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
    statements: []Statement,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};
// --------------------------------------------------------------------

pub const Type = struct {
    token: Token,
    value: Identifier,
};

pub const Float = struct {
    token: Token,
    value: f64,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Integer = struct {
    token: Token,
    value: i64,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const String = struct {
    token: Token,
    value: []const u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn typeIs() BuiltinType {
        return .boolean;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Array = struct {
    token: Token,
    elements: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Range = struct {
    token: Token,
    start: *Expression,
    end: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Hash = struct {
    token: Token,
    pairs: std.AutoHashMap(*Expression, *Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const EnumLiteral = struct {
    token: Token,
    tags: std.StringHashMap(*Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const EnumTag = struct {
    token: Token,
    value: Identifier,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionStatement = struct {
    token: Token,
    name: Identifier,
    func: Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Defer = struct {
    token: Token,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Function = struct {
    parameters: []Identifier,
    body: Block,
    token: Token,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Call = struct {
    token: Token, // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Method = struct {
    token: Token, // .
    caller: *Expression, //
    method: Identifier,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};
const ForLoopMode = enum { infinity, decl_infinity, decl_cond, dec_cond_inc, cond, cond_inc, range, multi_range };
// for i = 0, i < 10, i++, i in 0..10
pub const ForLoopExpression = struct {
    token: Token,
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
        ident: []const u8,
        index: ?[]const u8 = null,
        iterable: *Expression,
    };

    loops: []LoopVars,
    body: Block,
    token: Token,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ForLoopRange = struct {
    token: Token,
    ident: []const u8,
    index: ?[]const u8 = null,
    iterable: *Expression,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const MatchChoices = struct {
    token: Token, // =>
    exps: ?[]const Expression,
    block: Block,
};

pub const Match = struct {
    token: Token,
    value: *Expression, // swtich (value)
    choices: []MatchChoices,

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
        args_type: []const TypeLiteral,
        return_type: TypeLiteral,
    },
};
