const std = @import("std");
const Token = @import("Token.zig");

pub const None = struct {
    pub fn tokenLiteral() []const u8 {}
};

pub const Statement = struct {
    node: None,
    pub fn statementNone() void {}
};

pub const Expression = struct {
    node: None,
    pub fn expressionNone() void {}
};

//--------------------------------------------

pub const Identifier = struct {
    token: Token, //= .identifier,
    value: []const u8,

    pub fn expressionNone(self: *const Identifier) void {
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

pub const VarStatement = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: Expression,

    pub fn statementNone(self: *const VarStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const VarStatement) []const u8 {
        return self.token.literal;
    }
};
