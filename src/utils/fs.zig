const std = @import("std");

pub const FS = struct {
    pub fn read(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
        return std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    }
};
