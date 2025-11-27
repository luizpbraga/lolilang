const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const loli = b.addModule("loli", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const exe = b.addExecutable(.{
        .name = "loli",
        .root_module = loli,
        .use_llvm = false,
        .use_lld = false,
    });
    b.installArtifact(exe);
}
