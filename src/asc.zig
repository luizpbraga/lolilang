const std = @import("std");
const Token = @import("Token.zig");

// interface
pub const Node = union {
    expression: Expression,
    statement: Statement,

    fn tokenLiteral(self: *Node) []const u8 {
        return switch (self) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    fn string(self: *Node) []const u8 {
        return switch (self) {
            inline else => |node| node.tokenLiteral(),
        };
    }
};

/// implements Node,
/// interface: {return, var, expression} statements
pub const Statement = union {
    // implements Node,
    var_statement: VarStatement,
    return_statement: ReturnStatement,

    fn statementNode(self: *Statement) void {
        switch (self) {
            inline else => |x| x.statementNode(),
        }
    }

    // interface methods
    fn tokenLiteral() []const u8 {}
    fn string() []const u8 {}
};

/// implements Node,
pub const Expression = union {
    fn expressionNode() void {}

    // interface methods
    fn tokenLiteral() []const u8 {}
    fn string() []const u8 {}
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
    value: Expression,

    pub fn statementNode(self: *const VarStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const VarStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: Expression,

    pub fn statementNode(self: *const ReturnStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ExpressionStatement = struct {
    token: Token, // fist token only
    expression: Expression,

    pub fn statementNode(self: *const ExpressionStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }
};
