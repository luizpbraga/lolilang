const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const object = @import("object.zig");
const eval = @import("evaluator.zig").eval;

const Loli = struct {
    fn run(allocator: std.mem.Allocator, input: []const u8) !void {
        var lexer = Lexer.init(input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        var env = object.Environment.init(allocator);
        defer env.deinit();

        _ = try eval(allocator, .{ .statement = .{ .program_statement = program } }, &env);
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var args = std.process.args();
    _ = args.next();

    var file_name = if (args.next()) |file| file else return error.MissingFILENAME;
    std.debug.print("{s}\n", .{file_name});

    const input: []const u8 = try std.fs.cwd().readFileAlloc(allocator, file_name, 1024);
    defer allocator.free(input);

    try Loli.run(allocator, input);

    std.debug.print("\n\t** loli gos brrr **\n", .{});
}
