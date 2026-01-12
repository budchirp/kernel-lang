const std = @import("std");

const lexer_zig = @import("lexer.zig");
const source_zig = @import("source.zig");
const token_type_zig = @import("token/token_type.zig");

const Lexer = lexer_zig.Lexer;
const Source = source_zig.Source;
const TokenType = token_type_zig.TokenType;

test "operators" {
    const source = Source.init("test", "= + - * / % < > == != <= >= && || ++ --");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]TokenType{
        .Assign,
        .Plus,
        .Minus,
        .Asterisk,
        .Divide,
        .Modulo,
        .LessThan,
        .GreaterThan,
        .Equals,
        .NotEquals,
        .LessEqual,
        .GreaterEqual,
        .And,
        .Or,
        .Increment,
        .Decrement,
        .EOF,
    };

    for (expected) |expected_type| {
        const token = lexer.next_token();
        try std.testing.expectEqual(expected_type, token.type);
    }
}

test "delimiters" {
    const source = Source.init("test", "{ } ( ) [ ] . : , ;");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]TokenType{
        .LeftBrace,
        .RightBrace,
        .LeftParen,
        .RightParen,
        .LeftBracket,
        .RightBracket,
        .Dot,
        .Colon,
        .Comma,
        .Semicolon,
        .EOF,
    };

    for (expected) |expected_type| {
        const token = lexer.next_token();
        try std.testing.expectEqual(expected_type, token.type);
    }
}

test "keywords" {
    const source = Source.init("test", "import struct class fn return var if for mut new while operator delete private public is");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]TokenType{
        .Import,
        .Struct,
        .Class,
        .Fn,
        .Return,
        .Var,
        .If,
        .For,
        .Mut,
        .New,
        .While,
        .Operator,
        .Delete,
        .Private,
        .Public,
        .Init,
        .Deinit,
        .Is,
        .EOF,
    };

    for (expected) |expected_type| {
        const token = lexer.next_token();
        try std.testing.expectEqual(expected_type, token.type);
    }
}

test "boolean literals" {
    const source = Source.init("test", "true false true");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]struct { token_type: TokenType, literal: []const u8 }{
        .{ .token_type = .Boolean, .literal = "true" },
        .{ .token_type = .Boolean, .literal = "false" },
        .{ .token_type = .Boolean, .literal = "true" },
        .{ .token_type = .EOF, .literal = "" },
    };

    for (expected) |item| {
        const token = lexer.next_token();
        try std.testing.expectEqual(item.token_type, token.type);
        try std.testing.expectEqualStrings(item.literal, token.literal);
    }
}

test "identifiers" {
    const source = Source.init("test", "foo bar _test snake_case camelCase");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected_literals = [_][]const u8{ "foo", "bar", "_test", "snake_case", "camelCase" };

    for (expected_literals) |literal| {
        const token = lexer.next_token();
        try std.testing.expectEqual(TokenType.Identifier, token.type);
        try std.testing.expectEqualStrings(literal, token.literal);
    }
}

test "numbers" {
    const source = Source.init("test", "42 123 3.14 0.5");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]struct { token_type: TokenType, literal: []const u8 }{
        .{ .token_type = .Number, .literal = "42" },
        .{ .token_type = .Number, .literal = "123" },
        .{ .token_type = .Float, .literal = "3.14" },
        .{ .token_type = .Float, .literal = "0.5" },
    };

    for (expected) |item| {
        const token = lexer.next_token();
        try std.testing.expectEqual(item.token_type, token.type);
        try std.testing.expectEqualStrings(item.literal, token.literal);
    }
}

test "strings" {
    const source = Source.init("test", "\"hello\" \"world\" \"\"");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_][]const u8{ "hello", "world", "" };

    for (expected) |literal| {
        const token = lexer.next_token();
        try std.testing.expectEqual(TokenType.String, token.type);
        try std.testing.expectEqualStrings(literal, token.literal);
    }
}

test "comments" {
    const source = Source.init("test", "var x // this is a comment\nvar y");
    var lexer = Lexer.init(std.testing.allocator, source);

    try std.testing.expectEqual(TokenType.Var, lexer.next_token().type);
    try std.testing.expectEqualStrings("x", lexer.next_token().literal);
    try std.testing.expectEqual(TokenType.Var, lexer.next_token().type);
    try std.testing.expectEqualStrings("y", lexer.next_token().literal);
}

test "variable statement" {
    const source = Source.init("test", "var mut x: int = 42");
    var lexer = Lexer.init(std.testing.allocator, source);

    const expected = [_]struct { token_type: TokenType, literal: []const u8 }{
        .{ .token_type = .Var, .literal = "var" },
        .{ .token_type = .Mut, .literal = "mut" },
        .{ .token_type = .Identifier, .literal = "x" },
        .{ .token_type = .Colon, .literal = ":" },
        .{ .token_type = .Identifier, .literal = "int" },
        .{ .token_type = .Assign, .literal = "=" },
        .{ .token_type = .Number, .literal = "42" },
        .{ .token_type = .EOF, .literal = "" },
    };

    for (expected) |item| {
        const token = lexer.next_token();
        try std.testing.expectEqual(item.token_type, token.type);
        try std.testing.expectEqualStrings(item.literal, token.literal);
    }
}

test "position tracking" {
    const source = Source.init("test", "var\nx");
    var lexer = Lexer.init(std.testing.allocator, source);

    var token = lexer.next_token();
    try std.testing.expectEqual(@as(u32, 1), token.position.line);
    try std.testing.expectEqual(@as(u32, 1), token.position.column);

    token = lexer.next_token();
    try std.testing.expectEqual(@as(u32, 2), token.position.line);
    try std.testing.expectEqual(@as(u32, 1), token.position.column);
}

test "illegal tokens" {
    const source = Source.init("test", "@ # $");
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(TokenType.Illegal, lexer.next_token().type);
    try std.testing.expectEqual(TokenType.Illegal, lexer.next_token().type);
    try std.testing.expectEqual(TokenType.Illegal, lexer.next_token().type);
    try std.testing.expectEqual(TokenType.EOF, lexer.next_token().type);
}
