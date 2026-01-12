const std = @import("std");

const token_zig = @import("token/token.zig");
const token_type_zig = @import("token/token_type.zig");
const position_zig = @import("position.zig");
const keywords_zig = @import("token/keywords.zig");
const source_zig = @import("source.zig");

const Token = token_zig.Token;
const TokenType = token_type_zig.TokenType;
const Position = position_zig.Position;
const Source = source_zig.Source;
const Keywords = keywords_zig.Keywords;

pub const Lexer = struct {
    input: []const u8,

    byte_position: usize,
    read_position: usize,

    ch: u8,
    position: Position,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: Source) Lexer {
        var lexer = Lexer{
            .input = source.content,
            .byte_position = 0,
            .read_position = 0,
            .ch = 0,
            .position = Position.init(1, 0),
            .allocator = allocator,
        };

        lexer.read_char();

        return lexer;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.byte_position = self.read_position;
        self.read_position += 1;

        if (self.ch == '\n') {
            self.position.line += 1;
            self.position.column = 0;
        } else {
            self.position.column += 1;
        }
    }

    fn peek_char(self: *const Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }

        return self.input[self.read_position];
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\r' or self.ch == '\n') {
            self.read_char();
        }
    }

    fn read_identifier(self: *Lexer) []const u8 {
        const start = self.byte_position;
        while (is_letter(self.ch) or is_digit(self.ch) or self.ch == '_') {
            self.read_char();
        }

        return self.input[start..self.byte_position];
    }

    fn read_number(self: *Lexer) TokenType {
        var is_float = false;

        while (is_digit(self.ch)) {
            self.read_char();
        }

        if (self.ch == '.' and is_digit(self.peek_char())) {
            is_float = true;
            self.read_char();
            while (is_digit(self.ch)) {
                self.read_char();
            }
        }

        return if (is_float) TokenType.Float else TokenType.Number;
    }

    fn read_string(self: *Lexer) []const u8 {
        var string = std.ArrayListUnmanaged(u8){};

        self.read_char();
        while (self.ch != '"' and self.ch != 0) {
            if (self.ch == '\\') {
                self.read_char();
                switch (self.ch) {
                    'n' => string.append(self.allocator, '\n') catch {},
                    'r' => string.append(self.allocator, '\r') catch {},
                    't' => string.append(self.allocator, '\t') catch {},
                    '\\' => string.append(self.allocator, '\\') catch {},
                    '"' => string.append(self.allocator, '"') catch {},
                    else => {
                        string.append(self.allocator, '\\') catch {};
                        string.append(self.allocator, self.ch) catch {};
                    },
                }
            } else {
                string.append(self.allocator, self.ch) catch {};
            }
            self.read_char();
        }

        if (self.ch == '"') {
            self.read_char();
        }

        return string.toOwnedSlice(self.allocator) catch "";
    }

    fn skip_comment(self: *Lexer) void {
        while (self.ch != '\n' and self.ch != 0) {
            self.read_char();
        }
    }

    pub fn next_token(self: *Lexer) Token {
        self.skip_whitespace();

        const position = self.position;

        var token_type: TokenType = undefined;
        var literal: []const u8 = undefined;

        switch (self.ch) {
            '=' => {
                if (self.peek_char() == '=') {
                    literal = "==";
                    token_type = TokenType.Equals;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = "=";
                    token_type = TokenType.Assign;

                    self.read_char();
                }
            },
            '+' => {
                if (self.peek_char() == '+') {
                    literal = "++";
                    token_type = TokenType.Increment;
                    self.read_char();
                    self.read_char();
                } else {
                    literal = "+";
                    token_type = TokenType.Plus;
                    self.read_char();
                }
            },
            '-' => {
                if (self.peek_char() == '-') {
                    literal = "--";
                    token_type = TokenType.Decrement;
                    self.read_char();
                    self.read_char();
                } else {
                    literal = "-";
                    token_type = TokenType.Minus;
                    self.read_char();
                }
            },
            '*' => {
                literal = "*";
                token_type = TokenType.Asterisk;

                self.read_char();
            },
            '/' => {
                if (self.peek_char() == '/') {
                    self.skip_comment();
                    return self.next_token();
                }

                literal = "/";
                token_type = TokenType.Divide;

                self.read_char();
            },
            '%' => {
                literal = "%";
                token_type = TokenType.Modulo;

                self.read_char();
            },
            '<' => {
                if (self.peek_char() == '=') {
                    literal = "<=";
                    token_type = TokenType.LessEqual;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = "<";
                    token_type = TokenType.LessThan;

                    self.read_char();
                }
            },
            '>' => {
                if (self.peek_char() == '=') {
                    literal = ">=";
                    token_type = TokenType.GreaterEqual;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = ">";
                    token_type = TokenType.GreaterThan;

                    self.read_char();
                }
            },
            '!' => {
                if (self.peek_char() == '=') {
                    literal = "!=";
                    token_type = TokenType.NotEquals;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = "!";
                    token_type = TokenType.Not;

                    self.read_char();
                }
            },
            '&' => {
                if (self.peek_char() == '&') {
                    literal = "&&";
                    token_type = TokenType.And;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = "&";
                    token_type = TokenType.Ampersand;

                    self.read_char();
                }
            },
            '|' => {
                if (self.peek_char() == '|') {
                    literal = "||";
                    token_type = TokenType.Or;

                    self.read_char();
                    self.read_char();
                } else {
                    literal = "|";
                    token_type = TokenType.Illegal;

                    self.read_char();
                }
            },
            '{' => {
                literal = "{";
                token_type = TokenType.LeftBrace;

                self.read_char();
            },
            '}' => {
                literal = "}";
                token_type = TokenType.RightBrace;

                self.read_char();
            },
            '(' => {
                literal = "(";
                token_type = TokenType.LeftParen;

                self.read_char();
            },
            ')' => {
                literal = ")";
                token_type = TokenType.RightParen;

                self.read_char();
            },
            '[' => {
                literal = "[";
                token_type = TokenType.LeftBracket;

                self.read_char();
            },
            ']' => {
                literal = "]";
                token_type = TokenType.RightBracket;

                self.read_char();
            },
            '.' => {
                if (self.peek_char() == '.' and self.byte_position + 2 < self.input.len and self.input[self.byte_position + 2] == '.') {
                    literal = "...";
                    token_type = TokenType.Ellipsis;

                    self.read_char();
                    self.read_char();
                    self.read_char();
                } else {
                    literal = ".";
                    token_type = TokenType.Dot;

                    self.read_char();
                }
            },
            ':' => {
                literal = ":";
                token_type = TokenType.Colon;

                self.read_char();
            },
            ',' => {
                literal = ",";
                token_type = TokenType.Comma;

                self.read_char();
            },
            ';' => {
                literal = ";";
                token_type = TokenType.Semicolon;

                self.read_char();
            },
            '"' => {
                literal = self.read_string();
                token_type = TokenType.String;
            },
            0 => {
                literal = "";
                token_type = TokenType.EOF;
            },
            else => {
                if (is_letter(self.ch)) {
                    literal = self.read_identifier();
                    token_type = Keywords.get(literal) orelse TokenType.Identifier;
                } else if (is_digit(self.ch)) {
                    const start = self.byte_position;
                    token_type = self.read_number();
                    literal = self.input[start..self.byte_position];
                } else {
                    literal = "";
                    token_type = TokenType.Illegal;
                    self.read_char();
                }
            },
        }

        return Token.init(token_type, literal, position);
    }

    fn is_letter(ch: u8) bool {
        return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
    }

    fn is_digit(ch: u8) bool {
        return ch >= '0' and ch <= '9';
    }
};
