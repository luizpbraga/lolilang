const std = @import("std");
const Token = @import("Token.zig");

const allocator = std.heap.page_allocator;

// interface
pub const Node = union {
    expression: Expression,
    statement: Statement,

    fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    fn string(self: *const Node) []const u8 {
        return switch (self.*) {
            .statements => |n| n.string(),
            .expression => |n| n.string(),
        };
    }
};

/// implements Node,
/// interface: {return, var, expression} statements
pub const Statement = union(enum) {
    // implements Node,
    var_statement: VarStatement,
    return_statement: ReturnStatement,

    fn statementNode(self: *const Statement) void {
        switch (self.*) {
            inline else => |x| x.statementNode(),
        }
    }

    // interface methods
    fn tokenLiteral(self: *const Statement) []const u8 {
        _ = self;
    }

    fn string(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |x| x.string(),
        };
    }
};

/// implements Node,
pub const Expression = union(enum) {
    identifier: Identifier,

    fn expressionNode(self: *const Expression) void {
        _ = self;
    }

    // interface methods
    fn tokenLiteral(self: *const Expression) []const u8 {
        _ = self;
    }

    fn string(self: *const Expression) []const u8 {
        return self.identifier.string();
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

    pub fn string(self: *const Identifier) []const u8 {
        return self.value;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const Program) []const u8 {
        return if (self.statements.items.len > 0) self.statements.items[0].tokenLiteral() else "";
    }

    /// caller must free the memory
    pub fn string(self: *const Program) []const u8 {
        var buff_list = std.ArrayList(u8).init(allocator);
        errdefer buff_list.deinit();

        for (self.statements.items) |*stmt| {
            const stmt_string = stmt.string();
            buff_list.writer().writeAll(stmt_string) catch unreachable;
        }

        return buff_list.toOwnedSlice() catch unreachable;
    }
};

// ------------------------------------------------------------------------

pub const VarStatement = struct {
    token: Token, //= .@"var",
    name: Identifier,
    value: ?Expression,

    pub fn statementNode(self: *const VarStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const VarStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const VarStatement) []const u8 {
        return if (self.value) |value|
            std.fmt.allocPrint(allocator, "{s} {s} = {s};", .{
                self.tokenLiteral(),
                self.name.string(),
                value.string(),
            }) catch unreachable
        else
            std.fmt.allocPrint(allocator, "{s} {s};", .{
                self.tokenLiteral(),
                self.name.string(),
            }) catch unreachable;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?Expression,

    pub fn statementNode(self: *const ReturnStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ReturnStatement) []const u8 {
        return if (self.value) |value|
            std.fmt.allocPrint(allocator, "{s} {s};", .{ self.tokenLiteral(), value.string() }) catch unreachable
        else
            std.fmt.allocPrint(allocator, "{s};", .{self.tokenLiteral()}) catch unreachable;
    }
};

pub const ExpressionStatement = struct {
    token: Token, // fist token only
    expression: ?Expression,

    pub fn statementNode(self: *const ExpressionStatement) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ExpressionStatement) []const u8 {
        if (self.expression) |exp| {
            return exp.string();
        }
        return "";
    }
};
