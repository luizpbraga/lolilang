const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Environment = @import("Environment.zig");

const Loli = struct {
    fn runInterpreter(allocator: std.mem.Allocator, input: []const u8) !void {
        var lexer = Lexer.init(input);

        var parser = Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = parser.parseProgram() catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            return;
        };

        var env = Environment.init(allocator);
        defer env.deinit();

        _ = try env.eval(.{ .statement = .{ .program_statement = program } });
    }
};

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    var args = std.process.args();
    _ = args.next();

    const file_name = if (args.next()) |file| file else return error.MissingFile;
    const input: []const u8 = try std.fs.cwd().readFileAlloc(allocator, file_name, 1024);
    defer allocator.free(input);

    try Loli.runInterpreter(allocator, input);
}
