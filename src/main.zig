const std = @import("std");

const cli_zig = @import("cli/cli.zig");

const CLI = cli_zig.CLI;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer {
    //     _ = gpa.deinit();
    // }

    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var cli = CLI.init(allocator, args);
    defer cli.deinit();

    try cli.run();
}
