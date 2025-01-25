const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const loli = b.addModule("loli", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const exe = b.addExecutable(.{
        .name = "loli",
        .root_module = loli,
        // .use_llvm = false,
        // .use_lld = false,
    });

    b.installArtifact(exe);

    // const unit_tests = b.addTest(.{
    //     .root_source_file = b.path("src/test.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // const run_unit_tests = b.addRunArtifact(unit_tests);
    //
    // const test_step = b.step("test", "Run unit tests");
    // test_step.dependOn(&run_unit_tests.step);
}
