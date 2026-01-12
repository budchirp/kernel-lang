const std = @import("std");

const token_type_zig = @import("token_type.zig");
const position_zig = @import("../position.zig");

const TokenType = token_type_zig.TokenType;
const Position = position_zig.Position;

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    position: Position,

    pub fn init(@"type": TokenType, literal: []const u8, position: Position) Token {
        return Token{
            .type = @"type",
            .literal = literal,
            .position = position,
        };
    }

    pub fn dump(self: Token) void {
        std.debug.print("Token(type: {s}, literal: '{s}', position: ", .{ @tagName(self.type), self.literal });
        self.position.dump();
        std.debug.print(")", .{});
    }
};
