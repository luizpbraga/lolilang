const std = @import("std");
const Token = @import("Token.zig");

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
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,
    program_statement: Program,

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
    boolean: Boolean,
    integer_literal: IntegerLiteral,
    string_literal: StringLiteral,
    array_literal: ArrayLiteral,
    identifier: Identifier,
    function_literal: FunctionLiteral,
    assignment_expression: AssignmentExpression,

    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,
    call_expression: CallExpression,
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

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }
};

pub const IndexExpression = struct {
    token: Token, // [
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(self: *const IndexExpression) []const u8 {
        return self.token.literal;
    }
};

//--------------------------------------------

pub const Identifier = struct {
    token: Token, //= .identifier,
    value: []const u8,

    pub fn expressionNode(self: *const Identifier) void {
        _ = self;
    }
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const Program) []const u8 {
        return if (self.statements.items.len > 0) self.statements.items[0].tokenLiteral() else "";
    }
};

// ------------------------------------------------------------------------
pub const ConstStatement = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: ?Expression = null,

    pub fn statementNode(self: *const ConstStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ConstStatement) []const u8 {
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
    value: ?Expression = null,

    pub fn statementNode(self: *const VarStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const VarStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?Expression = null,

    pub fn statementNode(self: *const ReturnStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
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

    pub fn statementNode(self: *const AssignmentExpression) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const AssignmentExpression) []const u8 {
        return self.token.literal;
    }
};

pub const ExpressionStatement = struct {
    token: Token, // fist token only
    expression: *Expression,

    pub fn statementNode(self: *const ExpressionStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []Statement,

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }
};
// --------------------------------------------------------------------

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }
};

pub const StringLiteral = struct {
    token: Token,
    value: []const u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const StringLiteral) []const u8 {
        return self.token.literal;
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: []Expression,

    pub fn tokenLiteral(self: *const ArrayLiteral) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: []Identifier,
    body: BlockStatement,

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }
};

pub const CallExpression = struct {
    token: Token, // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []Expression,

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }
};

pub const MethodExpression = struct {
    token: Token, // .
    caller: *Expression, //
    method: Identifier,

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }
};
