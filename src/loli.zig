const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Environment = @import("Environment.zig");
const Compiler = @import("Compiler.zig");
const buildins = @import("buildins.zig");
const Vm = @import("Vm.zig");

// pub fn startRepl(allocator: anytype) !void {
//     const stdin = std.io.getStdIn();
//     const stdout = std.io.getStdOut();
//     var writer = stdout.writer();
//     var compiler: Compiler = .init(allocator);
//     defer compiler.deinit();
//
//     var buffer: [500]u8 = undefined;
//     try stdout.writeAll(">>> ");
//     while (try stdin.reader().readUntilDelimiterOrEof(&buffer, '\n')) |input| {
//         if (std.mem.eql(u8, input, "\\lc")) {
//             for (compiler.constants.items) |con| try writer.print("\t{}\n", .{con});
//             try writer.writeAll("\n>>> ");
//             continue;
//         }
//
//         if (std.mem.eql(u8, input, "\\lg")) {
//             for (compiler.symbols.store.keys()) |con| try writer.print("\t{s}\n", .{con});
//             try writer.writeAll("\n>>> ");
//             continue;
//         }
//
//         var lexer = Lexer.init(input);
//
//         var parser = Parser.new(allocator, &lexer);
//         defer parser.deinit();
//
//         const node = parser.parse() catch |err| {
//             std.log.err("parser error: {s}\n>>>", .{@errorName(err)});
//             continue;
//         };
//
//         compiler.compile(node) catch |err| {
//             try writer.print("compiler error: {s}\n>>> ", .{@errorName(err)});
//             continue;
//         };
//
//         // // assert the bytecodes
//         var b = try compiler.bytecode();
//         defer b.deinit(&compiler);
//
//         var vm = Vm.init(allocator, &b);
//         try vm.run();
//
//         const obj = vm.lastPopped();
//
//         // buildins.pprint(&obj);
//
//         try writer.print("{}\n>>", .{obj});
//
//         // std.debug.print("values:\n", .{});
//         // for (vm.stack) |value| {
//         //     std.debug.print("{} ", .{value});
//         // }
//
//         // std.debug.print("globals:\n", .{});
//         // for (vm.globals) |value| {
//         //     if (value) |v|
//         //         std.debug.print("{} ", .{v});
//         // }
//
//         // for (compiler.instructions.items) |ins| compiler.allocator.free(ins);
//     }
// }

pub fn runInterpreter(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    var env = Environment.init(allocator);
    defer env.deinit();

    _ = try env.eval(node);
}

pub fn runVm(allocator: std.mem.Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);

    var parser = Parser.new(allocator, &lexer);
    defer parser.deinit();

    const node = try parser.parse();

    var compiler: Compiler = try .init(allocator);
    defer compiler.deinit();

    try compiler.compile(node);

    // // assert the bytecodes
    var code = try compiler.bytecode();
    defer code.deinit(&compiler);

    var vm: Vm = try .init(allocator, &code);
    defer vm.deinit();

    try vm.run();

    const obj = vm.lastPopped();

    print(obj);
    std.debug.print("\n", .{});
}

fn print(obj: anytype) void {
    switch (obj) {
        .obj => |o| switch (o.type) {
            .string => |s| std.debug.print("{s}", .{s}),
            .array => |arr| {
                std.debug.print("[", .{});
                for (arr, 0..) |s, i| {
                    print(s);
                    if (i != arr.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print("]", .{});
            },
            else => {},
        },
        inline else => |o| std.debug.print("{}", .{o}),
    }
}
