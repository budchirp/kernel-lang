const std = @import("std");

const source_zig = @import("../lexer/source.zig");
const position_zig = @import("../lexer/position.zig");
pub const colors_zig = @import("colors.zig");
pub const error_kind_zig = @import("error_kind.zig");

const Source = source_zig.Source;
const Position = position_zig.Position;
const ErrorKind = error_kind_zig.ErrorKind;

pub const Logger = struct {
    source: Source,

    pub fn init(source: Source) Logger {
        return Logger{ .source = source };
    }

    pub fn log(_: Logger, message: []const u8) void {
        std.debug.print("{s}\n", .{message});
    }

    pub fn info(_: Logger, message: []const u8) void {
        std.debug.print("{s}{s}info{s}: {s}\n", .{
            colors_zig.bold,
            colors_zig.bright_blue,
            colors_zig.reset,
            message,
        });
    }

    pub fn warning(self: Logger, position: Position, message: []const u8) void {
        std.debug.print("\n{s}{s}warning{s}: {s}{s}{s}\n", .{
            colors_zig.bold,
            colors_zig.bright_yellow,
            colors_zig.reset,
            colors_zig.bold,
            message,
            colors_zig.reset,
        });
        self.print_location(position, colors_zig.bright_yellow);
    }

    pub fn err(self: Logger, kind: ErrorKind, position: Position, message: []const u8) void {
        std.debug.print("\n{s}{s}error{s}: {s}{s}{s}\n", .{
            colors_zig.bold,
            kind.color(),
            colors_zig.reset,
            colors_zig.bold,
            message,
            colors_zig.reset,
        });
        self.print_location(position, kind.color());
    }

    fn print_location(self: Logger, position: Position, highlight: []const u8) void {
        std.debug.print("  {s}-->{s} {s}:{d}:{d}\n", .{
            colors_zig.bright_blue,
            colors_zig.reset,
            self.source.name,
            position.line,
            position.column,
        });

        const line = self.get_line(position.line) orelse return;
        const padding = if (count_digits(position.line) < 4) 4 - count_digits(position.line) else 0;

        std.debug.print("  {s}", .{colors_zig.dim});
        print_spaces(padding);
        std.debug.print(" |{s}\n", .{colors_zig.reset});

        std.debug.print("  {s}{d} | {s}{s}\n", .{ colors_zig.cyan, position.line, colors_zig.reset, line });

        std.debug.print("  {s}", .{colors_zig.dim});
        print_spaces(padding);
        std.debug.print(" | {s}", .{colors_zig.reset});
        print_spaces(position.column - 1);
        std.debug.print("{s}{s}^{s}\n", .{ colors_zig.bold, highlight, colors_zig.reset });

        std.debug.print("  {s}", .{colors_zig.dim});
        print_spaces(padding);
        std.debug.print(" |{s}\n\n", .{colors_zig.reset});
    }

    fn get_line(self: Logger, line_num: u32) ?[]const u8 {
        var current_line: u32 = 1;
        var start: usize = 0;

        for (self.source.content, 0..) |char, i| {
            if (current_line == line_num) {
                var end = i;
                while (end < self.source.content.len and self.source.content[end] != '\n') {
                    end += 1;
                }
                return self.source.content[start..end];
            }

            if (char == '\n') {
                current_line += 1;
                start = i + 1;
            }
        }

        if (current_line == line_num and start < self.source.content.len) {
            return self.source.content[start..];
        }

        return null;
    }

    fn print_spaces(count: u32) void {
        for (0..count) |_| {
            std.debug.print(" ", .{});
        }
    }

    fn count_digits(n: u32) u32 {
        if (n == 0) return 1;
        var count: u32 = 0;
        var num = n;
        while (num > 0) {
            count += 1;
            num /= 10;
        }
        return count;
    }
};
