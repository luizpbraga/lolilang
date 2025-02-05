const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const stderr = std.io.getStdErr();
const Error = @import("Error.zig");

const Line = struct {
    input: []const u8,
    start: usize = 0,
    end: usize = 0,
    index: usize = 0,

    pub fn line(input: []const u8, position: usize) usize {
        const pos = if (position > input.len) input.len else position;
        const index = std.mem.count(u8, input[0..pos], "\n") + 1;
        return index;
    }
};

test {
    const input =
        \\x := struct {
        \\allocator
        \\fn foo() {}
        \\}
    ;
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

pub fn format(allocator: std.mem.Allocator, file_name: []const u8, input: []const u8, err: *Error) !void {
    var lexer: Lexer = .init(input);

    var parser: Parser = .init(allocator, &lexer, err);
    defer parser.deinit();

    const node = try parser.parse();

    if (parser.errors.msg.items.len != 0) {
        try stderr.writeAll(
            parser.errors.msg.items,
        );
        return;
    }

    var w: Write = .init(allocator, input);
    defer w.deinit();

    try w.write(node);

    var file = try std.fs.cwd().createFile(file_name, .{});
    defer file.close();

    try file.writeAll(w.buffer.items);
}

pub const Write = struct {
    input: []const u8,
    buffer: std.ArrayList(u8),
    block: usize = 0,
    line_number: usize = 1,

    pub const R = struct { start: usize, end: usize };

    fn init(allocator: anytype, input: []const u8) Write {
        return .{ .buffer = .init(allocator), .input = input };
    }

    fn deinit(w: *Write) void {
        w.buffer.deinit();
    }

    fn findLine(w: *Write, pos: usize) !void {
        const at = Line.line(w.input, pos);
        if (at > w.line_number) for (w.line_number..at) |_| {
            try w.newLine();
        };
    }

    fn writeAt(w: *Write, pos: usize, s: []const u8) !void {
        try w.buffer.insert(pos, s);
    }

    fn newLine(w: *Write) !void {
        w.line_number += 1;
        try w.append("\n");
    }

    fn print(w: *Write, comptime fmt: []const u8, args: anytype) !void {
        try w.buffer.writer().print(fmt, args);
    }

    fn append(w: *Write, slice: []const u8) !void {
        try w.buffer.appendSlice(slice);
    }

    fn tab(w: *Write) !void {
        try w.buffer.writer().writeByteNTimes(' ', 4 * w.block);
    }

    pub fn write(w: *Write, node: ast.Node) !void {
        if (node.commentPos()) |pos| {
            const comment = std.mem.trim(u8, w.input[pos[0]..pos[1]], " ");
            const n = std.mem.count(u8, comment, "\n");
            for (0..n) |_| _ = w.buffer.popOrNull();
            // w.line_number -= n;
            try w.print("{s}", .{comment});
            try w.tab();
        }

        switch (node) {
            .statement => |stmt| switch (stmt) {
                .program => |program| {
                    for (program.statements.items) |s| {
                        try w.findLine(s.position());
                        try w.write(.{ .statement = s });
                        try w.newLine();
                    }
                },

                .exp_statement => |exp_stmt| {
                    try w.write(.{ .expression = exp_stmt.expression });
                },

                .@"pub" => |p| {
                    try w.append("pub ");
                    try w.write(.{ .statement = p.stmt.* });
                },

                .import => |p| {
                    try w.append("import ");
                    try w.write(.{ .expression = p.path });
                },

                .block => |block| {
                    if (block.statements.len == 0) {
                        try w.append("{}");
                        return;
                    }

                    // if (block.statements.len == 1 and block.statements[0] != .@"return" and block.statements[0].commentPos() == null) {
                    //     try w.append("{ ");
                    //     try w.write(.{ .statement = block.statements[0] });
                    //     try w.append(" }");
                    //     // try w.newLine();
                    //     return;
                    // }

                    try w.append("{");
                    try w.newLine();
                    w.block += 1;
                    for (block.statements) |s| {
                        try w.findLine(s.position());
                        try w.tab();
                        try w.write(.{ .statement = s });
                        try w.newLine();
                    }
                    w.block -= 1;
                    try w.tab();
                    // try w.buffer.writer().writeByteNTimes('\t', w.block);
                    try w.append("}");
                },

                .@"var" => |var_stmt| {
                    try w.print("var {s} = ", .{var_stmt.name.value});
                    try w.write(.{ .expression = var_stmt.value });
                },

                .con => |var_stmt| {
                    try w.print("con {s} = ", .{var_stmt.name[0].value});
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
                    try w.newLine();
                },

                // .comment => |co| {
                //     try w.print("{s}", .{std.mem.trim(u8, w.input[co.at..co.end], " \n")});
                // },

                else => |a| {
                    std.debug.print("{}", .{a});
                    return error.SorryMamaaaaaaaaaaaaaaaaaa;
                },
            },

            .expression => |exp| switch (exp.*) {
                .identifier => |ident| {
                    try w.print("{s}", .{ident.value});
                },

                // TODO: not fully implemented
                .assignment => |assignment| {
                    try w.write(.{ .expression = assignment.name });
                    try w.print(" {s} ", .{@tagName(assignment.operator)});
                    try w.write(.{ .expression = assignment.value });
                },

                .group => |group| {
                    try w.append("(");
                    try w.write(.{ .expression = group.exp });
                    try w.append(")");
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

                // TODO: think !!
                .range => |range| {
                    try w.write(.{ .expression = range.start });
                    try w.append("..");
                    try w.write(.{ .expression = range.end });
                },

                .integer => |int| {
                    try w.print("{}", .{int.value});
                },
                .tag => |e| {
                    try w.print(".{s}", .{e.value});
                },
                .char => |e| {
                    switch (e.value) {
                        '\n' => try w.append("'\\n'"),
                        '\r' => try w.append("'\\r'"),
                        '\t' => try w.append("'\\t'"),
                        else => try w.print("'{c}'", .{e.value}),
                    }
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
                    try w.write(.{ .expression = instance.type });

                    if (instance.fields.len == 0) {
                        if (w.buffer.items[w.buffer.items.len - 2] == '}') {
                            _ = w.buffer.popOrNull();
                        }
                        try w.append("{}");
                        return;
                        // w.block += 1;
                    } else {
                        try w.append("{");
                        try w.newLine();
                        w.block += 1;
                    }

                    for (instance.fields) |field| {
                        try w.tab();
                        // try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.print("{s}: ", .{field.name.value});
                        try w.write(.{ .expression = field.value });
                        try w.append(",");
                        try w.newLine();
                    }
                    w.block -= 1;
                    try w.tab();
                    // try w.buffer.writer().writeByteNTimes('\t', w.block);
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
                    try w.append("if (");
                    try w.write(.{ .expression = ifexp.condition });
                    try w.append(") ");
                    try w.write(.{ .statement = .{ .block = ifexp.consequence } });
                    if (ifexp.alternative) |alt| {
                        try w.append(" else ");
                        try w.write(.{ .statement = .{ .block = alt } });
                    }
                },
                .match => |match| {
                    try w.append("match (");
                    try w.write(.{ .expression = match.value });
                    try w.append(") {");
                    try w.newLine();

                    w.block += 1;
                    for (match.arms) |arm| {
                        try w.tab();
                        // try w.buffer.writer().writeByteNTimes('\t', w.block);
                        for (arm.condition) |condition| {
                            try w.write(.{ .expression = condition });
                            try w.append(", ");
                        }
                        _ = w.buffer.popOrNull();
                        _ = w.buffer.popOrNull();
                        try w.append(" => ");
                        try w.write(.{ .statement = .{ .block = arm.block } });
                        try w.newLine();
                    }

                    if (match.else_block) |block| {
                        try w.tab();
                        // try w.buffer.writer().writeByteNTimes('\t', w.block);
                        try w.append("else => ");
                        try w.write(.{ .statement = .{ .block = block } });
                    }
                    w.block -= 1;
                    try w.newLine();
                    try w.tab();
                    // try w.buffer.writer().writeByteNTimes('\t', w.block);
                    try w.append("}");
                },
                .for_range => |forloop| {
                    try w.print("for ({s} in ", .{forloop.ident});
                    try w.write(.{ .expression = forloop.iterable });
                    try w.append(") ");
                    try w.write(.{ .statement = .{ .block = forloop.body } });
                },
                .@"for" => |forloop| {
                    try w.append("for (");
                    try w.write(.{ .expression = forloop.condition });
                    try w.append(") ");
                    try w.write(.{ .statement = .{ .block = forloop.consequence } });
                },
                .type => |ty| {
                    if (ty.fields.len == 0) {
                        try w.print("{s} {{}}", .{@tagName(ty.type)});
                        return;
                    }

                    try w.print("{s} {{", .{@tagName(ty.type)});
                    try w.newLine();
                    w.block += 1;

                    for (ty.fields) |field| {
                        try w.tab();
                        // try w.buffer.writer().writeByteNTimes('\t', w.block);
                        if (field.value.* == .null) {
                            try w.print("{s}", .{field.name.value});
                            try w.newLine();
                            continue;
                        }
                        try w.print("{s} = ", .{field.name.value});
                        try w.write(.{ .expression = field.value });
                        try w.newLine();
                    }

                    if (ty.decl.len != 0 and ty.fields.len != 0) {
                        try w.newLine();
                    }

                    if (ty.fields.len != 0 and ty.decl.len == 0) {
                        _ = w.buffer.pop();
                    }

                    for (ty.decl) |func_stmt| {
                        try w.tab();
                        // try w.buffer.writer().writeByteNTimes('\t', w.block);
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
                        try w.newLine();
                        try w.newLine();
                    }

                    if (ty.decl.len != 0) {
                        _ = w.buffer.pop();
                        _ = w.buffer.pop();
                    }

                    w.block -= 1;
                    try w.newLine();
                    try w.tab();
                    try w.append("}");
                    try w.newLine();
                },

                else => |a| {
                    std.debug.print("{}", .{a});
                    return error.A;
                },
            },
        }
    }
};
