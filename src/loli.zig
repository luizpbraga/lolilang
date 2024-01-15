const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Environment = @import("Environment.zig");
const GarbageCollector = @import("GarbageCollector.zig");

const Loli = struct {
    fn run(allocator: std.mem.Allocator, input: []const u8) !void {
        var lexer = Lexer.init(input);

        var parser = try Parser.new(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var gc = GarbageCollector.init(allocator);
        defer gc.deinit();

        // var t = try gc.start();
        // defer t.join();

        var env = Environment.init(allocator, &gc);
        defer env.deinit();

        _ = try env.eval(.{ .statement = .{ .program_statement = program } });

        // gc.exit = true;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (.leak == gpa.deinit()) std.debug.print("LEAK", .{});

    var args = std.process.args();
    _ = args.next();

    const file_name = if (args.next()) |file| file else return error.MissingFILENAME;

    const input: []const u8 = try std.fs.cwd().readFileAlloc(allocator, file_name, 1024);
    defer allocator.free(input);

    try Loli.run(allocator, input);

    std.debug.print("\n\t** loli go brrr **\n", .{});
}
