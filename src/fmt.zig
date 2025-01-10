const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const stderr = std.io.getStdErr();

test {
    const input = @embedFile("./fmt.loli");
    // const input =
    //     \\if true { var x = 33 } else { var y = 22 }
    // ;
    const allocator = std.testing.allocator;

    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    if (parser.errors.counter != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var w: Write = .init(allocator);
    defer w.deinit();

    try w.write(node);

    std.debug.print("{s}", .{w.buffer.items});
}

const Write = struct {
    buffer: std.ArrayList(u8),
    block: usize = 0,

    fn init(allocator: anytype) Write {
        return .{ .buffer = .init(allocator) };
    }

    fn deinit(w: *Write) void {
        w.buffer.deinit();
    }

    fn print(w: *Write, comptime fmt: []const u8, args: anytype) !void {
        try w.buffer.writer().print(fmt, args);
    }

    fn append(w: *Write, slice: []const u8) !void {
        try w.buffer.appendSlice(slice);
    }

    pub fn write(w: *Write, node: ast.Node) !void {
        switch (node) {
            .statement => |stmt| switch (stmt) {
                .program => |program| {
                    for (program.statements.items) |s| {
                        try w.write(.{ .statement = s });
                        try w.append("\n");
                    }
                },

                .exp_statement => |exp_stmt| {
                    try w.write(.{ .expression = exp_stmt.expression });
                },

                .block => |block| {
                    try w.append("{\n");
                    w.block += 1;
                    for (block.statements) |_stmt| {
                        try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.write(.{ .statement = _stmt });
                        try w.append("\n");
                    }
                    w.block -= 1;
                    try w.buffer.writer().writeByteNTimes('\t', w.block);
                    try w.append("}");
                },

                .@"var" => |var_stmt| {
                    try w.print("var {s} = ", .{var_stmt.name.value});
                    try w.write(.{ .expression = var_stmt.value });
                },

                .@"return" => |ret| {
                    try w.append("return ");
                    try w.write(.{ .expression = ret.value });
                },

                // TODO: make it work in if/match/for
                .@"break" => |ret| {
                    try w.append("break ");
                    try w.write(.{ .expression = ret.value });
                },

                .@"continue" => |ret| {
                    try w.append("continue ");
                    try w.write(.{ .expression = ret.value });
                },

                .@"fn" => |func_stmt| {
                    try w.print("fn {s}(", .{func_stmt.name.value});
                    const func = func_stmt.func;
                    for (func.parameters) |param| {
                        try w.print("{s}, ", .{param.value});
                    }
                    if (func.parameters.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }
                    try w.append(") ");

                    try w.write(.{ .statement = .{ .block = func.body } });
                },

                else => {},
            },

            .expression => |exp| switch (exp.*) {
                .identifier => |ident| {
                    try w.print("{s}", .{ident.value});
                },

                // TODO: not fully implemented
                .assignment => |assignment| {
                    try w.write(.{ .expression = assignment.value });
                },

                .infix => |infix| {
                    try w.write(.{ .expression = infix.left });
                    try w.print(" {s} ", .{@tagName(infix.operator)});
                    try w.write(.{ .expression = infix.right });
                },
                //
                .prefix => |prefix| {
                    try w.print("{s}", .{@tagName(prefix.operator)});
                    try w.write(.{ .expression = prefix.right });
                },
                //
                .index => |index| {
                    try w.write(.{ .expression = index.left });
                    try w.append("[");
                    try w.write(.{ .expression = index.index });
                    try w.append("]");
                },
                //
                // // TODO: rework
                .method => |method| {
                    try w.write(.{ .expression = method.caller });
                    try w.print(".{s}", .{method.method.value});
                },

                .boolean => |boolean| {
                    try w.print("{}", .{boolean.value});
                },

                .null => {
                    try w.append("null");
                },

                // // TODO: think !!
                // .range => |range| {
                //     try c.write(.{ .expression = range.end });
                // },

                .integer => |int| {
                    try w.print("{}", .{int.value});
                },

                .tag => |e| {
                    try w.print(".{s}", .{e.value});
                },

                .char => |e| {
                    try w.print("'{c}'", .{e.value});
                },

                .float => |e| {
                    try w.print("{d}", .{e.value});
                },

                .string => |str| {
                    try w.print("\"{s}\"", .{str.value});
                },

                .array => |array| {
                    try w.append("[");
                    for (array.elements) |element| {
                        try w.write(.{ .expression = element });
                        try w.append(", ");
                    }

                    if (array.elements.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }
                    try w.append("]");
                },

                .hash => |hash| {
                    try w.append("{");
                    for (hash.pairs) |pair| {
                        const key, const val = pair;
                        try w.write(.{ .expression = key });
                        try w.append(": ");
                        try w.write(.{ .expression = val });
                        try w.append(", ");
                    }
                    if (hash.pairs.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }
                    try w.append("}");
                },

                .call => |call| {
                    try w.write(.{ .expression = call.function });
                    try w.append("(");
                    for (call.arguments) |arg| {
                        try w.write(.{ .expression = arg });
                        try w.append(", ");
                    }

                    if (call.arguments.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }
                    try w.append(")");
                },

                .instance => |instance| {
                    try w.append("new ");
                    try w.write(.{ .expression = instance.type });

                    if (instance.fields.len == 0) {
                        try w.append("{");
                        w.block += 1;
                    } else {
                        try w.append("{\n");
                        w.block += 1;
                    }

                    for (instance.fields) |field| {
                        try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.print("{s}: ", .{field.name.value});
                        try w.write(.{ .expression = field.value });
                        try w.append(",\n");
                    }
                    w.block -= 1;
                    try w.buffer.writer().writeByteNTimes('\t', w.block);
                    try w.append("}");
                },

                .function => |func| {
                    try w.print("fn(", .{});
                    for (func.parameters) |param| {
                        try w.print("{s}, ", .{param.value});
                    }
                    if (func.parameters.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }
                    try w.append(") ");

                    try w.write(.{ .statement = .{ .block = func.body } });
                },

                .@"if" => |ifexp| {
                    try w.append("if ");
                    try w.write(.{ .expression = ifexp.condition });
                    try w.append(" ");
                    try w.write(.{ .statement = .{ .block = ifexp.consequence } });
                    if (ifexp.alternative) |alt| {
                        try w.append(" else ");
                        try w.write(.{ .statement = .{ .block = alt } });
                    }
                },

                .match => |match| {
                    try w.append("match ");
                    try w.write(.{ .expression = match.value });
                    try w.append(" {\n");

                    w.block += 1;
                    for (match.arms) |arm| {
                        try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.write(.{ .expression = arm.condition });
                        try w.append(" => ");
                        try w.write(.{ .statement = .{ .block = arm.block } });
                        try w.append("\n");
                    }

                    if (match.else_block) |block| {
                        try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.append("else => ");
                        try w.write(.{ .statement = .{ .block = block } });
                    }
                    w.block -= 1;
                    try w.append("\n");
                    try w.buffer.writer().writeByteNTimes('\t', w.block);
                    try w.append("}");
                },
                .@"for" => |forloop| {
                    try w.append("for ");
                    try w.write(.{ .expression = forloop.condition });
                    try w.append(" ");
                    try w.write(.{ .statement = .{ .block = forloop.consequence } });
                },
                //
                // // TODO: make it like a function
                // .type => |ty| {
                //     _ = ty;
                // },

                else => {},
            },
        }
    }
};
