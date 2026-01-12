const std = @import("std");

pub const Source = struct {
    name: []const u8,
    content: []const u8,

    pub fn init(name: []const u8, content: []const u8) Source {
        return Source{
            .name = name,
            .content = content,
        };
    }

    pub fn dump(self: Source) void {
        std.debug.print("Source(name: '{s}', content: '{s}')", .{ self.name, self.content });
    }
};
