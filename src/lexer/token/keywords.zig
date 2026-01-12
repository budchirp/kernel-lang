const std = @import("std");

const token_type_zig = @import("token_type.zig");

const TokenType = token_type_zig.TokenType;

pub const Keywords = struct {
    pub fn get(ident: []const u8) ?TokenType {
        if (std.mem.eql(u8, ident, "import")) return TokenType.Import;
        if (std.mem.eql(u8, ident, "struct")) return TokenType.Struct;
        if (std.mem.eql(u8, ident, "class")) return TokenType.Class;
        if (std.mem.eql(u8, ident, "fn")) return TokenType.Fn;
        if (std.mem.eql(u8, ident, "operator")) return TokenType.Operator;
        if (std.mem.eql(u8, ident, "delete")) return TokenType.Delete;
        if (std.mem.eql(u8, ident, "private")) return TokenType.Private;
        if (std.mem.eql(u8, ident, "public")) return TokenType.Public;
        if (std.mem.eql(u8, ident, "return")) return TokenType.Return;
        if (std.mem.eql(u8, ident, "var")) return TokenType.Var;
        if (std.mem.eql(u8, ident, "if")) return TokenType.If;
        if (std.mem.eql(u8, ident, "else")) return TokenType.Else;
        if (std.mem.eql(u8, ident, "for")) return TokenType.For;
        if (std.mem.eql(u8, ident, "mut")) return TokenType.Mut;
        if (std.mem.eql(u8, ident, "new")) return TokenType.New;
        if (std.mem.eql(u8, ident, "while")) return TokenType.While;
        if (std.mem.eql(u8, ident, "true")) return TokenType.Boolean;
        if (std.mem.eql(u8, ident, "false")) return TokenType.Boolean;
        if (std.mem.eql(u8, ident, "is")) return TokenType.Is;
        if (std.mem.eql(u8, ident, "extern")) return TokenType.Extern;
        if (std.mem.eql(u8, ident, "as")) return TokenType.As;

        return null;
    }
};
