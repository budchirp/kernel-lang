const std = @import("std");

pub fn build(context: *std.Build) void {
    const target = context.standardTargetOptions(.{});
    const optimize = context.standardOptimizeOption(.{});

    const app_module = context.addExecutable(.{
        .name = "kernel_lang",
        .root_module = context.createModule(.{
            .root_source_file = context.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    app_module.root_module.addIncludePath(.{ .cwd_relative = "/usr/include/llvm-c" });
    app_module.root_module.linkSystemLibrary("LLVM", .{});

    context.installArtifact(app_module);

    const run_cmd = context.addRunArtifact(app_module);
    run_cmd.step.dependOn(context.getInstallStep());

    if (context.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = context.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_module = context.addTest(.{
        .root_module = context.createModule(.{
            .root_source_file = context.path("src/test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const test_step = context.step("test", "Run all tests");
    test_step.dependOn(&context.addRunArtifact(test_module).step);
}
