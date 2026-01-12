const std = @import("std");

const colors_zig = @import("colors.zig");

pub const ErrorKind = enum {
    LexerError,
    ParserError,
    TypeError,
    UndefinedSymbol,
    RuntimeError,

    pub fn dump(self: ErrorKind) void {
        std.debug.print("{s}", .{switch (self) {
            .LexerError => "Lexer Error",
            .ParserError => "Parser Error",
            .TypeError => "Type Error",
            .UndefinedSymbol => "Undefined Symbol",
            .RuntimeError => "Runtime Error",
        }});
    }

    pub fn color(self: ErrorKind) []const u8 {
        return switch (self) {
            .LexerError => colors_zig.bright_red,
            .ParserError => colors_zig.bright_red,
            .TypeError => colors_zig.bright_yellow,
            .UndefinedSymbol => colors_zig.bright_red,
            .RuntimeError => colors_zig.bright_magenta,
        };
    }
};
