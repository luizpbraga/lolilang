const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "loli",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe.linkLibC();
    exe.linkSystemLibrary("gc");

    b.installArtifact(exe);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("test/test.zig"),
        .target = target,
        .optimize = optimize,
    });

    const compiler_tests = b.addTest(.{
        .root_source_file = b.path("src/Compiler.zig"),
        .target = target,
        .optimize = optimize,
    });

    compiler_tests.linkLibC();

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const run_compiler_tests = b.addRunArtifact(compiler_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const compiler_tests_step = b.step("compiler_test", "Run Compiler Test");
    compiler_tests_step.dependOn(&run_compiler_tests.step);
}
