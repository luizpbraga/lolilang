const std = @import("std");
const Token = @import("Token.zig");

const TypeLiteral = union(enum) {
    null,
    bool,
    int,
    uint,
    float,
    char,

    string: struct {
        len: usize,
    },

    array: struct {
        len: usize,
        type: *const TypeLiteral,
    },

    structure,
    enumerator,
};

// interface
pub const Node = union(enum) {
    expression: Expression,
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
    var_statement: VarStatement,
    var_block_statement: VarBlockStatement,
    const_statement: ConstStatement,
    const_block_statement: ConstBlockStatement,
    return_statement: ReturnStatement,
    break_statement: BreakStatement,
    defer_statement: DeferStatement,
    function_statement: FunctionStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,
    program_statement: Program,
    struct_statement: StructStatement,

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

/// implements Node,
pub const Expression = union(enum) {
    null_literal,
    type: TypeLiteral,
    boolean: Boolean,
    float_literal: FloatLiteral,
    integer_literal: IntegerLiteral,
    string_literal: StringLiteral,
    char_literal: CharLiteral,
    array_literal: ArrayLiteral,
    range: RangeExpression,
    hash_literal: HashLiteral,
    enum_literal: EnumLiteral,
    struct_literal: StructLiteral,
    enum_tag: EnumTag,
    identifier: Identifier,
    function_literal: FunctionLiteral,

    assignment_expression: AssignmentExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,

    if_expression: IfExpression,
    forloop_expression: ForLoopExpression,
    forloop_range_expression: ForLoopRangeExpression,
    multi_forloop_range_expression: MultiForLoopRangeExpression,
    switch_expression: SwitchExpression,

    call_expression: CallExpression,
    init_expression: InitExpression,
    method_expression: MethodExpression,
    index_expression: IndexExpression,

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

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement = null,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const IndexExpression = struct {
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
pub const ConstStatement = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const VarBlockStatement = struct {
    token: Token, //= .@"var",
    vars_decl: []VarStatement,
};

pub const ConstBlockStatement = struct {
    token: Token, //= .@"var",
    const_decl: []ConstStatement,
};

pub const VarStatement = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: Expression,
    type: ?*Expression = null,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const BreakStatement = struct {
    token: Token,
    value: Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

// identifier = expression
//  = , +=, -=, *=, /=,
pub const AssignmentExpression = struct {
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

pub const ExpressionStatement = struct {
    token: Token, // fist token only
    expression: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const BlockStatement = struct {
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

pub const FloatLiteral = struct {
    token: Token,
    value: f64,
    type: TypeLiteral = .float,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,
    type: TypeLiteral = .int,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const CharLiteral = struct {
    token: Token,
    value: u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const StringLiteral = struct {
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

    pub fn typeIs() TypeLiteral {
        return .bool;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: []Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const RangeExpression = struct {
    token: Token,
    start: *Expression,
    end: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const HashLiteral = struct {
    token: Token,
    pairs: std.AutoHashMap(*const Expression, *Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

/// con P = struct {}
pub const StructLiteral = struct {
    token: Token, // .{
    fields: std.StringHashMap(*Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

/// struct P {}
pub const StructStatement = struct {
    token: Token,
    name: Identifier,
    fields: std.StringHashMap(*Expression),

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

pub const DeferStatement = struct {
    token: Token,
    body: BlockStatement,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionLiteral = struct {
    parameters: []Identifier,
    body: BlockStatement,
    token: Token,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const CallExpression = struct {
    token: Token, // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const InitExpression = struct {
    token: Token, // (
    type: *Expression, // Identifier of StructLiteral
    struct_: StructLiteral,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const MethodExpression = struct {
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
    consequence: BlockStatement,
    mode: ForLoopMode = .infinity,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const MultiForLoopRangeExpression = struct {
    pub const LoopVars = struct {
        ident: []const u8,
        index: ?[]const u8 = null,
        iterable: *Expression,
    };

    loops: []LoopVars,
    body: BlockStatement,
    token: Token,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ForLoopRangeExpression = struct {
    token: Token,
    ident: []const u8,
    index: ?[]const u8 = null,
    iterable: *Expression,
    body: BlockStatement,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const SwitchChoices = struct {
    token: Token, // =>
    exps: ?[]const Expression,
    block: BlockStatement,
};

pub const SwitchExpression = struct {
    token: Token,
    value: *Expression, // swtich (value)
    choices: []SwitchChoices,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};
