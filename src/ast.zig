const std = @import("std");
const Token = @import("Token.zig");

// interface
pub const Node = union {
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
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

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
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    function_literal: FunctionLiteral,
    boolean: Boolean,

    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,

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
    condition: ?*Expression = null,
    consequence: ?BlockStatement = null,
    alternative: ?BlockStatement = null,

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: ?*Expression = null,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: ?*Expression = null,
    operator: []const u8,
    right: ?*Expression = null,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
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

pub const ExpressionStatement = struct {
    token: Token, // fist token only
    expression: ?*Expression = null,

    pub fn statementNode(self: *const ExpressionStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(Statement),

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

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: ?std.ArrayList(*Identifier) = null,
    body: ?BlockStatement = null,

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }
};
