const std = @import("std");
const Token = @import("Token.zig");

// interface
pub const Node = union(enum) {
    expression: *Expression,
    // pointer? more heap allocation...
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
    @"continue": Continue,

    @"defer": Defer,

    block: Block,

    @"fn": FunctionStatement,
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
    condition: *Expression,
    consequence: Block,
    alternative: ?Block = null,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "if";
    }
};

pub const Prefix = struct {
    operator: Token.Type,
    right: *Expression,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return @tagName(self.operator);
    }
};

pub const Infix = struct {
    left: *Expression,
    operator: Token.Type,
    right: *Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return @tagName(self.operator);
    }
};

pub const Index = struct {
    // [
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "[";
    }
};

//--------------------------------------------

pub const Identifier = struct {
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
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return if (self.statements.items.len > 0) self.statements.items[0].tokenLiteral() else "";
    }
};

// ------------------------------------------------------------------------
pub const Con = struct {
    //= .@"var",
    name: Identifier,
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "con";
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
    //= .@"var",
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
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "return";
    }
};

pub const Continue = struct {
    value: *Expression,

    pub fn statementNode(self: *const @This()) void {
        _ = self;
    }

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "continue";
    }
};

pub const Break = struct {
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
    statements: []Statement,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "{";
    }
};
// --------------------------------------------------------------------

pub const Float = struct {
    value: f32,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Integer = struct {
    value: i32,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        var buf: [10]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{}", .{self.value}) catch "";
    }
};

pub const Char = struct {
    value: u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.value;
    }
};

pub const String = struct {
    value: []const u8,

    pub fn expressionNode() void {}

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.value;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn typeIs() BuiltinType {
        return .boolean;
    }

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return if (self.value) "true" else "false";
    }
};

pub const Array = struct {
    elements: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Range = struct {
    start: *Expression,
    end: *Expression,
    inc: enum { yes, no } = .no,

    pub fn tokenLiteral(_: *const @This()) []const u8 {
        return "..";
    }
};

// pub const Hash = struct {
//     pairs: std.AutoHashMap(*Expression, *Expression),
//
//     pub fn tokenLiteral(self: *const @This()) []const u8 {
//         return self.token.literal;
//     }
// };

pub const Hash = struct {
    pairs: [][2]*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const EnumLiteral = struct {
    tags: std.StringHashMap(*Expression),

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Tag = struct {
    value: []const u8,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const FunctionStatement = struct {
    name: Identifier,
    func: Function,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Defer = struct {
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Function = struct {
    name: ?Identifier = null,
    parameters: []Identifier,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Call = struct {
    // (
    function: *Expression, // Identifier or FunctionLiteral
    arguments: []*Expression,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Type = struct {
    type: Token.Type,
    name: ?[]const u8 = null,
    fields: []Field = &.{},
    desc: []FunctionStatement = &.{},

    pub const Field = struct {
        name: Identifier,
        value: *Expression,
    };

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Instance = struct {
    type: *Expression, // Identifier or FunctionLiteral
    fields: []Type.Field,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Method = struct {
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

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const ForRange = struct {
    ident: []const u8,
    index: ?[]const u8 = null,
    iterable: *Expression,
    body: Block,

    pub fn tokenLiteral(self: *const @This()) []const u8 {
        return self.token.literal;
    }
};

pub const Match = struct {
    value: *Expression,
    arms: []Arm,
    else_block: ?Block = null,

    pub const Arm = struct {
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
        args_type: []const TypeLiteral,
        return_type: TypeLiteral,
    },
};
