const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // TODO: override the installation dir
    const infos: []const struct { name: []const u8, path: []const u8 } = &.{
        .{ .name = "loli", .path = "src/main.zig" },
        // .{ .name = "loli_inter", .path = "src/interpreter.zig" },
    };

    for (infos) |info| {
        const exe = b.addExecutable(.{
            .name = info.name,
            .root_source_file = b.path(info.path),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        });

        b.installArtifact(exe);

        // exe.linkSystemLibrary("gc");
    }

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/test.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
