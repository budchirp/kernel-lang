const std = @import("std");

pub const Position = struct {
    line: u32,
    column: u32,

    pub fn init(line: u32, column: u32) Position {
        return Position{
            .line = line,
            .column = column,
        };
    }

    pub fn dump(self: Position) void {
        std.debug.print("Position({d}, {d})", .{ self.line, self.column });
    }
};
