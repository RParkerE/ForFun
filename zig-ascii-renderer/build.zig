const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Add executable
    const exe = b.addExecutable(.{
        .name = "zig-ascii-renderer",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    // Create run command for the executable
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Add unit tests for main.zig
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_main_tests = b.addRunArtifact(main_tests);

    // Add unit tests for renderer.zig
    const renderer_tests = b.addTest(.{
        .root_source_file = b.path("src/renderer.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_renderer_tests = b.addRunArtifact(renderer_tests);

    // Add unit tests for particle_system.zig
    const particle_system_tests = b.addTest(.{
        .root_source_file = b.path("src/particle_system.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_particle_system_tests = b.addRunArtifact(particle_system_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_main_tests.step);
    test_step.dependOn(&run_renderer_tests.step);
    test_step.dependOn(&run_particle_system_tests.step);
}
