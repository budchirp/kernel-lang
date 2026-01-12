const std = @import("std");

pub fn create(allocator: std.mem.Allocator, comptime T: type, init: T) !*T {
    const ptr = try allocator.create(T);
    ptr.* = init;

    return ptr;
}
