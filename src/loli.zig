const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Loli = struct {
    fn run(allocator: std.mem.Allocator, input: []const u8) !void {
        var lexer = Lexer.init(input);
        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram(allocator);
        defer program.statements.deinit();

        for (program.statements.items) |x|
            std.debug.print("{}\n\n", .{x});
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var args = std.process.args();
    _ = args.next();

    var file_name = if (args.next()) |file| file else return error.MissingFILENAME;
    std.debug.print("{s}", .{file_name});

    const input: []const u8 = try std.fs.cwd().readFileAlloc(allocator, file_name, 1024);
    defer allocator.free(input);

    try Loli.run(allocator, input);

    std.debug.print("\n\t** loli gos brrr **\n", .{});
}
