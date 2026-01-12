const std = @import("std");

const lexer_zig = @import("../lexer/lexer.zig");
const token_zig = @import("../lexer/token/token.zig");
const token_type_zig = @import("../lexer/token/token_type.zig");
const position_zig = @import("../lexer/position.zig");
const source_zig = @import("../lexer/source.zig");
const node_zig = @import("../ast/node.zig");
const program_zig = @import("../ast/program.zig");
const logger_zig = @import("../logger/logger.zig");
const type_zig = @import("../types/type.zig");
const env_zig = @import("../symbol/env.zig");
const scope_zig = @import("../symbol/scope.zig");
const context_zig = @import("../compiler/context.zig");
const memory_zig = @import("../utils/memory.zig");

const Lexer = lexer_zig.Lexer;
const Token = token_zig.Token;
const Env = env_zig.Env;
const Scope = scope_zig.Scope;
const CompilerContext = context_zig.CompilerContext;
const TokenType = token_type_zig.TokenType;
const Position = position_zig.Position;
const Source = source_zig.Source;
const Node = node_zig.Node;
const Program = program_zig.Program;
const VariableStatement = node_zig.VariableStatement;
const FunctionStatement = node_zig.FunctionStatement;
const BlockStatement = node_zig.BlockStatement;
const Parameter = node_zig.Parameter;
const Identifier = node_zig.Identifier;
const NumberLiteral = node_zig.NumberLiteral;
const FloatLiteral = node_zig.FloatLiteral;
const StringLiteral = node_zig.StringLiteral;
const BooleanLiteral = node_zig.BooleanLiteral;
const TypeExpression = node_zig.TypeExpression;
const Visibility = node_zig.Visibility;
const ReturnStatement = node_zig.ReturnStatement;
const StructStatement = node_zig.StructStatement;
const StructStatementField = node_zig.StructStatementField;
const ExternStatement = node_zig.ExternStatement;
const ExternFnStatement = node_zig.ExternFnStatement;
const ExternVarStatement = node_zig.ExternVarStatement;
const BinaryExpression = node_zig.BinaryExpression;
const UnaryExpression = node_zig.UnaryExpression;
const FunctionProto = node_zig.FunctionProto;
const IfExpression = node_zig.IfExpression;
const ForStatement = node_zig.ForStatement;
const WhileStatement = node_zig.WhileStatement;
const ImportStatement = node_zig.ImportStatement;
const CallExpression = node_zig.CallExpression;
const MemberExpression = node_zig.MemberExpression;
const IndexExpression = node_zig.IndexExpression;
const Logger = logger_zig.Logger;
const ErrorKind = logger_zig.error_kind_zig.ErrorKind;
const Type = type_zig.Type;
const GenericParameter = node_zig.GenericParameter;
const GenericArgument = node_zig.GenericArgument;
const Argument = node_zig.Argument;
const ArrayLiteral = node_zig.ArrayLiteral;
const ExpressionStatement = node_zig.ExpressionStatement;
const StructLiteral = node_zig.StructLiteral;
const StructLiteralField = node_zig.StructLiteralField;

const memory = memory_zig;

pub const ParserError = error{
    UnexpectedToken,
    OutOfMemory,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    lexer: *Lexer,

    context: CompilerContext,

    env: *Env,

    current_token: Token,
    peek_token: Token,

    pub fn init(
        allocator: std.mem.Allocator,
        lexer: *Lexer,
        context: CompilerContext,
    ) !Parser {
        const env = try memory.create(allocator, Env, try Env.init(allocator));

        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
            .context = context,
            .env = env,
            .current_token = undefined,
            .peek_token = undefined,
        };

        parser.next_token();
        parser.next_token();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }

    fn next_token(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    fn expect_token(self: *Parser, token_type: TokenType) ParserError!void {
        if (self.current_token.type == token_type) {
            self.next_token();
            return;
        }

        const message = std.fmt.allocPrint(self.allocator, "expected '{s}', found '{s}'", .{
            @tagName(token_type),
            @tagName(self.current_token.type),
        }) catch "unexpected token";
        defer self.allocator.free(message);

        self.context.logger.err(ErrorKind.ParserError, self.current_token.position, message);

        return error.UnexpectedToken;
    }

    pub fn parse_program(self: *Parser) ParserError!Program {
        const position = self.current_token.position;

        var statements = std.ArrayListUnmanaged(Node){};
        while (self.current_token.type != TokenType.EOF) {
            const statement = try self.parse_statement();

            try statements.append(self.allocator, statement);

            if (self.current_token.type == TokenType.Semicolon) {
                self.next_token();
            }
        }

        return Program{
            .allocator = self.allocator,
            .env = self.env,
            .position = position,
            .statements = statements,
        };
    }

    fn parse_statement(self: *Parser) ParserError!Node {
        var visibility = Visibility.Public;
        switch (self.current_token.type) {
            .Private => {
                visibility = Visibility.Private;
                self.next_token();
            },
            .Public => {
                visibility = Visibility.Public;
                self.next_token();
            },
            else => {},
        }

        return switch (self.current_token.type) {
            .Import => self.parse_import_statement(),
            .Var => self.parse_variable_statement(visibility),
            .Fn => self.parse_function_statement(visibility),
            .Return => self.parse_return_statement(),
            .Struct => self.parse_struct_statement(visibility),
            .Extern => self.parse_extern_statement(visibility),
            .If => self.parse_if_expression(),
            .While => self.parse_while_statement(),
            .For => self.parse_for_statement(),
            .Identifier => self.parse_expression_statement(),
            else => self.parse_expression_statement(),
        };
    }

    fn parse_expression(self: *Parser) ParserError!Node {
        return self.parse_binary_expression(0);
    }

    fn parse_primary_expression(self: *Parser) ParserError!Node {
        return switch (self.current_token.type) {
            .Identifier => blk: {
                const identifier = try self.parse_identifier();

                break :blk Node{ .Identifier = identifier };
            },
            .Number => {
                const number = NumberLiteral{
                    .position = self.current_token.position,
                    .value = self.current_token.literal,
                };

                self.next_token();

                return Node{ .NumberLiteral = number };
            },
            .Float => {
                const float = FloatLiteral{
                    .position = self.current_token.position,
                    .value = self.current_token.literal,
                };

                self.next_token();

                return Node{ .FloatLiteral = float };
            },
            .String => {
                const string = StringLiteral{
                    .allocator = self.allocator,
                    .position = self.current_token.position,
                    .value = self.current_token.literal,
                };

                self.next_token();

                return Node{ .StringLiteral = string };
            },
            .Boolean => {
                const boolean = BooleanLiteral{
                    .position = self.current_token.position,
                    .value = std.mem.eql(u8, self.current_token.literal, "true"),
                };

                self.next_token();

                return Node{ .BooleanLiteral = boolean };
            },
            .LeftParen => {
                self.next_token();

                const expression = try self.parse_expression();
                try self.expect_token(TokenType.RightParen);

                return expression;
            },
            .LeftBracket => self.parse_array_literal(),
            .If => self.parse_if_expression(),
            .Delete, .New, .Asterisk => self.parse_unary_expression(),
            else => {
                self.context.logger.err(
                    ErrorKind.ParserError,
                    self.current_token.position,
                    "unknown expression",
                );

                return error.UnexpectedToken;
            },
        };
    }

    fn parse_import_statement(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Import);

        var path = std.ArrayListUnmanaged(Identifier){};

        const first_part = try self.parse_identifier();
        try path.append(self.allocator, first_part);

        while (self.current_token.type == TokenType.Dot) {
            self.next_token();

            const part = try self.parse_identifier();
            try path.append(self.allocator, part);
        }

        return Node{
            .ImportStatement = ImportStatement{
                .allocator = self.allocator,
                .position = position,
                .path = path,
            },
        };
    }

    fn parse_expression_statement(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        return Node{
            .ExpressionStatement = ExpressionStatement{
                .allocator = self.allocator,
                .position = position,
                .expression = try memory.create(self.allocator, Node, try self.parse_expression()),
            },
        };
    }

    fn parse_return_statement(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Return);

        return Node{
            .ReturnStatement = ReturnStatement{
                .allocator = self.allocator,
                .position = position,
                .value = try memory.create(self.allocator, Node, try self.parse_expression()),
            },
        };
    }

    fn parse_if_expression(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.If);

        try self.expect_token(TokenType.LeftParen);

        const condition = try memory.create(self.allocator, Node, try self.parse_expression());

        try self.expect_token(TokenType.RightParen);

        const consequence = try memory.create(self.allocator, Node, try self.parse_block_or_expression());

        var alternative: ?*Node = null;
        if (self.current_token.type == TokenType.Else) {
            self.next_token();

            if (self.current_token.type == TokenType.If) {
                alternative = try memory.create(self.allocator, Node, try self.parse_if_expression());
            } else {
                alternative = try memory.create(self.allocator, Node, try self.parse_block_or_expression());
            }
        }

        return Node{
            .IfExpression = IfExpression{
                .allocator = self.allocator,
                .position = position,
                .condition = condition,
                .consequence = consequence,
                .alternative = alternative,
            },
        };
    }

    fn parse_array_literal(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.LeftBracket);

        var elements = std.ArrayListUnmanaged(Node){};

        while (self.current_token.type != TokenType.RightBracket) {
            const element = try self.parse_expression();
            try elements.append(self.allocator, element);

            if (self.current_token.type == TokenType.Comma) {
                self.next_token();
            }
        }

        try self.expect_token(TokenType.RightBracket);

        return Node{
            .ArrayLiteral = ArrayLiteral{
                .allocator = self.allocator,
                .position = position,
                .elements = elements,
            },
        };
    }

    fn parse_while_statement(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.While);

        try self.expect_token(TokenType.LeftParen);

        const env = self.env;

        const scope = try memory.create(self.allocator, Scope, try Scope.init(self.allocator, "while", env.current_scope));
        env.set_current_scope(scope);

        const condition = try memory.create(self.allocator, Node, try self.parse_expression());

        try self.expect_token(TokenType.RightParen);

        const body = try self.parse_block_statement(scope);

        env.set_current_scope(scope.parent);

        return Node{
            .WhileStatement = WhileStatement{
                .allocator = self.allocator,
                .position = position,
                .condition = condition,
                .body = body,
            },
        };
    }

    fn parse_for_statement(self: *Parser) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.For);

        try self.expect_token(TokenType.LeftParen);

        const env = self.env;

        const scope = try memory.create(self.allocator, Scope, try Scope.init(self.allocator, "for", env.current_scope));
        env.set_current_scope(scope);

        var initializer: ?*Node = null;
        if (self.current_token.type != TokenType.Semicolon) {
            if (self.current_token.type == TokenType.Var) {
                initializer = try memory.create(self.allocator, Node, try self.parse_variable_statement(Visibility.Private));
            } else {
                initializer = try memory.create(self.allocator, Node, try self.parse_expression());
            }
        }

        try self.expect_token(TokenType.Semicolon);

        var condition: ?*Node = null;
        if (self.current_token.type != TokenType.Semicolon) {
            condition = try memory.create(self.allocator, Node, try self.parse_expression());
        }

        try self.expect_token(TokenType.Semicolon);

        var update: ?*Node = null;
        if (self.current_token.type != TokenType.RightParen) {
            update = try memory.create(self.allocator, Node, try self.parse_expression());
        }

        try self.expect_token(TokenType.RightParen);

        const body = try self.parse_block_statement(scope);

        env.set_current_scope(scope.parent);

        return Node{
            .ForStatement = ForStatement{
                .allocator = self.allocator,
                .position = position,
                .init = initializer,
                .condition = condition,
                .update = update,
                .body = body,
            },
        };
    }

    fn parse_block_or_expression(self: *Parser) ParserError!Node {
        if (self.current_token.type == TokenType.LeftBrace) {
            return Node{ .BlockStatement = try self.parse_block_statement(null) };
        }

        return self.parse_expression();
    }

    fn parse_binary_expression(self: *Parser, min_precedence: u8) ParserError!Node {
        var left = try self.parse_unary_expression();

        while (true) {
            const precedence = self.get_precedence(self.current_token.type);
            if (precedence == 0 or precedence < min_precedence) break;

            const operator = self.current_token;

            self.next_token();

            var right: Node = undefined;
            if (operator.type == .As) {
                right = Node{ .TypeExpression = try self.parse_type_expression() };
            } else {
                right = try self.parse_binary_expression(precedence + 1);
            }

            left = Node{
                .BinaryExpression = BinaryExpression{
                    .allocator = self.allocator,
                    .position = operator.position,
                    .left = try memory.create(self.allocator, Node, left),
                    .operator = operator,
                    .right = try memory.create(self.allocator, Node, right),
                },
            };
        }

        return left;
    }

    fn parse_call_expression(self: *Parser, callee: Node) ParserError!Node {
        const position = self.current_token.position;

        const generics = try self.parse_generic_arguments();

        try self.expect_token(TokenType.LeftParen);

        const arguments = try self.parse_arguments();

        return Node{
            .CallExpression = CallExpression{
                .allocator = self.allocator,
                .position = position,
                .callee = try memory.create(self.allocator, Node, callee),
                .generics = generics,
                .arguments = arguments,
            },
        };
    }

    fn parse_member_expression(self: *Parser, object: Node) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Dot);

        const property = try self.parse_identifier();

        return Node{
            .MemberExpression = MemberExpression{
                .allocator = self.allocator,
                .position = position,
                .object = try memory.create(self.allocator, Node, object),
                .property = property,
            },
        };
    }

    fn parse_index_expression(self: *Parser, array: Node) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.LeftBracket);

        const index = try self.parse_expression();

        try self.expect_token(TokenType.RightBracket);

        return Node{
            .IndexExpression = IndexExpression{
                .allocator = self.allocator,
                .position = position,
                .array = try memory.create(self.allocator, Node, array),
                .index = try memory.create(self.allocator, Node, index),
            },
        };
    }

    fn parse_unary_expression(self: *Parser) ParserError!Node {
        return switch (self.current_token.type) {
            .Minus, .Asterisk, .Ampersand, .Not, .Delete, .New => {
                const operator = self.current_token;

                self.next_token();

                const expression = try self.parse_postfix_expression();

                return Node{
                    .UnaryExpression = UnaryExpression{
                        .allocator = self.allocator,
                        .position = operator.position,
                        .operator = operator,
                        .operand = try memory.create(self.allocator, Node, expression),
                    },
                };
            },
            else => self.parse_postfix_expression(),
        };
    }

    fn parse_postfix_expression(self: *Parser) ParserError!Node {
        var left = try self.parse_primary_expression();

        while (true) {
            if (self.current_token.type == TokenType.LeftParen or self.current_token.type == TokenType.LessThan) {
                if (self.current_token.type == TokenType.LessThan) {
                    if (left != .Identifier) {
                        break;
                    }
                }

                left = try self.parse_call_expression(left);
            } else if (self.current_token.type == TokenType.Dot) {
                left = try self.parse_member_expression(left);
            } else if (self.current_token.type == TokenType.LeftBracket) {
                left = try self.parse_index_expression(left);
            } else if (self.current_token.type == TokenType.LeftBrace) {
                left = try self.parse_struct_literal(left);
            } else if (self.current_token.type == TokenType.Increment or self.current_token.type == TokenType.Decrement) {
                const operator = self.current_token;

                self.next_token();

                left = Node{
                    .UnaryExpression = UnaryExpression{
                        .allocator = self.allocator,
                        .position = operator.position,
                        .operator = operator,
                        .operand = try memory.create(self.allocator, Node, left),
                    },
                };
            } else {
                break;
            }
        }

        return left;
    }

    fn parse_variable_statement(self: *Parser, visibility: Visibility) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Var);

        var is_mut = false;
        if (self.current_token.type == TokenType.Mut) {
            is_mut = true;
            self.next_token();
        }

        const name = try self.parse_identifier();

        var @"type": ?TypeExpression = null;
        if (self.current_token.type == TokenType.Colon) {
            self.next_token();
            @"type" = try self.parse_type_expression();
        }

        var initializer: ?*Node = null;
        if (self.current_token.type == TokenType.Assign) {
            self.next_token();

            initializer = try memory.create(self.allocator, Node, try self.parse_expression());
        }

        return Node{
            .VariableStatement = VariableStatement{
                .allocator = self.allocator,
                .position = position,
                .visibility = visibility,
                .name = name,
                .type = @"type",
                .initializer = initializer,
                .is_mut = is_mut,
            },
        };
    }

    fn parse_function_statement(self: *Parser, visibility: Visibility) ParserError!Node {
        const position = self.current_token.position;

        var is_operator = false;
        if (self.current_token.type == TokenType.Operator) {
            is_operator = true;

            self.next_token();
        }

        try self.expect_token(TokenType.Fn);

        const proto = try self.parse_function_proto(visibility);

        const env = self.env;

        const scope = try memory.create(self.allocator, Scope, try Scope.init(self.allocator, proto.name.value, env.current_scope));
        scope.type = .Function;

        env.set_current_scope(scope);

        const body = try self.parse_block_statement(scope);

        env.set_current_scope(scope.parent);

        return Node{
            .FunctionStatement = FunctionStatement{
                .allocator = self.allocator,
                .position = position,
                .proto = proto,
                .body = body,
            },
        };
    }

    fn parse_function_proto(self: *Parser, visibility: Visibility) ParserError!FunctionProto {
        const token = self.current_token;

        var name: Identifier = undefined;
        if (token.type == .Identifier) {
            name = try self.parse_identifier();
        } else {
            const operator = switch (token.type) {
                .Plus, .Minus, .Asterisk, .Divide, .Modulo, .Equals, .NotEquals, .LessThan, .GreaterThan, .LessEqual, .GreaterEqual => token.literal,

                else => {
                    self.context.logger.err(
                        ErrorKind.ParserError,
                        self.current_token.position,
                        "expected operator symbol",
                    );
                    return error.UnexpectedToken;
                },
            };

            name = Identifier{
                .position = token.position,
                .value = operator,
            };

            self.next_token();
        }

        const generics = try self.parse_generic_parameters();

        try self.expect_token(TokenType.LeftParen);

        const parameters = try self.parse_parameters();

        try self.expect_token(TokenType.Colon);

        const return_type = try self.parse_type_expression();

        return FunctionProto{
            .allocator = self.allocator,
            .visibility = visibility,
            .name = name,
            .generics = generics,
            .parameters = parameters,
            .return_type = return_type,
        };
    }

    fn parse_struct_statement(self: *Parser, visibility: Visibility) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Struct);

        const name = try self.parse_identifier();

        var parent: ?TypeExpression = null;
        if (self.current_token.type == TokenType.Colon) {
            self.next_token();
            parent = try self.parse_type_expression();
        }

        const generics = try self.parse_generic_parameters();

        try self.expect_token(TokenType.LeftBrace);

        var fields = std.ArrayListUnmanaged(StructStatementField){};

        while (self.current_token.type != TokenType.RightBrace) {
            const field = try self.parse_identifier();

            try self.expect_token(TokenType.Colon);

            const field_type = try self.parse_type_expression();

            try fields.append(self.allocator, StructStatementField{
                .position = field.position,
                .name = field,
                .type = field_type,
            });
        }

        try self.expect_token(TokenType.RightBrace);

        return Node{
            .StructStatement = StructStatement{
                .allocator = self.allocator,
                .position = position,
                .visibility = visibility,
                .name = name,
                .parent = parent,
                .generics = generics,
                .fields = fields,
            },
        };
    }

    fn parse_extern_statement(self: *Parser, visibility: Visibility) ParserError!Node {
        const position = self.current_token.position;

        try self.expect_token(TokenType.Extern);

        switch (self.current_token.type) {
            .Fn => {
                return self.parse_extern_fn_statement(position, visibility);
            },
            .Var => {
                return self.parse_extern_var_statement(position, visibility);
            },
            else => {
                self.context.logger.err(
                    ErrorKind.ParserError,
                    self.current_token.position,
                    "expected 'fn' or 'var' after 'extern'",
                );
                return error.UnexpectedToken;
            },
        }
    }

    fn parse_extern_fn_statement(self: *Parser, position: Position, visibility: Visibility) ParserError!Node {
        try self.expect_token(TokenType.Fn);

        const proto = try self.parse_function_proto(visibility);

        return Node{
            .ExternStatement = ExternStatement{
                .Function = ExternFnStatement{
                    .position = position,
                    .visibility = visibility,
                    .proto = proto,
                },
            },
        };
    }

    fn parse_extern_var_statement(self: *Parser, position: Position, visibility: Visibility) ParserError!Node {
        try self.expect_token(TokenType.Var);

        const name = try self.parse_identifier();

        try self.expect_token(TokenType.Colon);

        const @"type" = try self.parse_type_expression();

        return Node{
            .ExternStatement = ExternStatement{
                .Variable = ExternVarStatement{
                    .position = position,
                    .visibility = visibility,
                    .name = name,
                    .type = @"type",
                },
            },
        };
    }

    fn parse_struct_literal(self: *Parser, @"struct": Node) ParserError!Node {
        const generics = try self.parse_generic_arguments();

        try self.expect_token(TokenType.LeftBrace);

        var fields = std.ArrayListUnmanaged(StructLiteralField){};

        while (self.current_token.type != TokenType.RightBrace) {
            const field = try self.parse_identifier();

            try self.expect_token(TokenType.Colon);

            const value = try self.parse_expression();

            try fields.append(self.allocator, StructLiteralField{
                .allocator = self.allocator,
                .position = field.position,
                .name = field,
                .value = try memory.create(self.allocator, Node, value),
            });

            if (self.current_token.type == TokenType.Comma) {
                self.next_token();
            } else if (self.current_token.type != TokenType.RightBrace) {
                return error.UnexpectedToken;
            }
        }

        try self.expect_token(TokenType.RightBrace);

        return Node{
            .StructLiteral = StructLiteral{
                .allocator = self.allocator,
                .position = @"struct".position(),
                .@"struct" = try memory.create(self.allocator, Node, @"struct"),
                .generics = generics,
                .fields = fields,
            },
        };
    }

    fn parse_block_statement(self: *Parser, scope: ?*Scope) ParserError!BlockStatement {
        const position = self.current_token.position;

        try self.expect_token(TokenType.LeftBrace);

        const env = self.env;

        const block_scope = if (scope) |s| s else try memory.create(self.allocator, Scope, try Scope.init(self.allocator, "block", env.current_scope));
        env.set_current_scope(block_scope);

        var statements = std.ArrayListUnmanaged(Node){};
        while (self.current_token.type != TokenType.RightBrace and self.current_token.type != TokenType.EOF) {
            const statement = try self.parse_statement();
            try statements.append(self.allocator, statement);

            if (self.current_token.type == TokenType.Semicolon) {
                self.next_token();
            }
        }

        try self.expect_token(TokenType.RightBrace);

        env.set_current_scope(block_scope.parent);

        return BlockStatement{
            .allocator = self.allocator,
            .position = position,
            .scope = block_scope,
            .statements = statements,
        };
    }

    fn parse_type_expression(self: *Parser) ParserError!TypeExpression {
        const position = self.current_token.position;

        if (self.current_token.type == TokenType.Asterisk) {
            self.next_token();

            const inner_type = try self.parse_type_expression();

            return TypeExpression{
                .allocator = self.allocator,
                .position = position,
                .value = Type{ .Pointer = .{ .child = try memory.create(self.allocator, Type, inner_type.value) } },
            };
        }

        if (self.current_token.type != TokenType.Identifier) {
            self.context.logger.err(
                ErrorKind.ParserError,
                self.current_token.position,
                "expected type name",
            );
            return error.UnexpectedToken;
        }

        var base_type = self.parse_base_type(self.current_token.literal);

        self.next_token();

        if (self.current_token.type == TokenType.LessThan) {
            self.next_token();

            var generics = std.ArrayListUnmanaged(Type){};
            while (self.current_token.type != TokenType.GreaterThan) {
                self.next_token();

                const argument = try self.parse_type_expression();
                try generics.append(self.allocator, argument.value);

                if (self.current_token.type == TokenType.Comma) {
                    self.next_token();
                } else if (self.current_token.type != TokenType.GreaterThan) {
                    return error.UnexpectedToken;
                }
            }

            try self.expect_token(TokenType.GreaterThan);

            switch (base_type) {
                .Struct => |_| {},
                else => {},
            }
        }

        while (true) {
            if (self.current_token.type == TokenType.LeftBracket) {
                self.next_token();

                var size: ?usize = null;
                if (self.current_token.type == TokenType.Number) {
                    size = std.fmt.parseInt(usize, self.current_token.literal, 10) catch null;
                    self.next_token();
                }

                switch (self.current_token.type) {
                    .Number => {
                        size = std.fmt.parseInt(usize, self.current_token.literal, 10) catch null;
                        self.next_token();

                        try self.expect_token(TokenType.RightBracket);
                    },
                    .RightBracket => {
                        self.next_token();
                    },
                    else => {
                        return error.UnexpectedToken;
                    },
                }

                base_type = Type{ .Array = type_zig.ArrayType{ .child = try memory.create(self.allocator, Type, base_type), .size = size } };
            } else {
                break;
            }
        }

        return TypeExpression{ .allocator = self.allocator, .position = position, .value = base_type };
    }

    fn parse_base_type(self: *Parser, name: []const u8) Type {
        _ = self;

        if (std.mem.eql(u8, name, "void")) {
            return Type.void();
        } else if (std.mem.eql(u8, name, "string")) {
            return Type{ .String = {} };
        } else if (std.mem.eql(u8, name, "bool")) {
            return Type{ .Boolean = {} };
        } else if (name.len >= 2 and (name[0] == 'i' or name[0] == 'u')) {
            const is_unsigned = name[0] == 'u';
            if (std.fmt.parseInt(u16, name[1..], 10) catch null) |size| {
                return Type{ .Integer = type_zig.IntegerType{ .is_unsigned = is_unsigned, .size = size } };
            } else {}
        } else if (name.len >= 2 and name[0] == 'f') {
            if (std.fmt.parseInt(u16, name[1..], 10) catch null) |size| {
                return Type{ .Float = type_zig.FloatType{ .size = size } };
            } else {}
        }

        return Type{ .Named = .{ .name = name } };
    }

    fn parse_identifier(self: *Parser) ParserError!Identifier {
        if (self.current_token.type != TokenType.Identifier) {
            self.context.logger.err(
                ErrorKind.ParserError,
                self.current_token.position,
                "expected identifier",
            );
            return error.UnexpectedToken;
        }

        const identifier = Identifier{
            .position = self.current_token.position,
            .value = self.current_token.literal,
        };

        self.next_token();

        return identifier;
    }

    fn parse_generic_parameters(self: *Parser) ParserError!std.ArrayListUnmanaged(GenericParameter) {
        var generics = std.ArrayListUnmanaged(GenericParameter){};

        if (self.current_token.type == TokenType.LessThan) {
            self.next_token();

            while (self.current_token.type != TokenType.GreaterThan) {
                const name = try self.parse_identifier();

                var constraint: ?TypeExpression = null;
                if (self.current_token.type == TokenType.Colon) {
                    self.next_token();
                    constraint = try self.parse_type_expression();
                }

                try generics.append(self.allocator, GenericParameter{
                    .position = name.position,
                    .name = name,
                    .constraint = constraint,
                });

                if (self.current_token.type == TokenType.Comma) {
                    self.next_token();
                }
            }

            try self.expect_token(TokenType.GreaterThan);
        }

        return generics;
    }

    fn parse_generic_arguments(self: *Parser) ParserError!std.ArrayListUnmanaged(GenericArgument) {
        var generics = std.ArrayListUnmanaged(GenericArgument){};

        if (self.current_token.type == TokenType.LessThan) {
            self.next_token();

            while (self.current_token.type != TokenType.GreaterThan) {
                const @"type" = try self.parse_type_expression();
                try generics.append(self.allocator, GenericArgument{
                    .position = @"type".position,
                    .name = null,
                    .type = @"type",
                });

                if (self.current_token.type == TokenType.Comma) {
                    self.next_token();
                }
            }

            try self.expect_token(TokenType.GreaterThan);
        }

        return generics;
    }

    fn parse_arguments(self: *Parser) ParserError!std.ArrayListUnmanaged(Argument) {
        var arguments = std.ArrayListUnmanaged(Argument){};
        while (self.current_token.type != TokenType.RightParen) {
            const argument = try self.parse_expression();

            try arguments.append(self.allocator, Argument{
                .allocator = self.allocator,
                .position = argument.position(),
                .name = null,
                .value = try memory.create(self.allocator, Node, argument),
            });

            if (self.current_token.type == TokenType.Comma) {
                self.next_token();
            }
        }

        try self.expect_token(TokenType.RightParen);

        return arguments;
    }

    fn parse_parameters(self: *Parser) ParserError!std.ArrayListUnmanaged(Parameter) {
        var parameters = std.ArrayListUnmanaged(Parameter){};

        while (self.current_token.type != TokenType.RightParen) {
            const parameter = try self.parse_parameter();
            try parameters.append(self.allocator, parameter);

            if (self.current_token.type == TokenType.Comma) {
                self.next_token();
            }
        }

        try self.expect_token(TokenType.RightParen);

        return parameters;
    }

    fn parse_parameter(self: *Parser) ParserError!Parameter {
        var is_variadic = false;
        if (self.current_token.type == TokenType.Ellipsis) {
            is_variadic = true;
            self.next_token();
        }

        const name = try self.parse_identifier();

        try self.expect_token(TokenType.Colon);

        const @"type" = try self.parse_type_expression();

        return Parameter{
            .position = name.position,
            .name = name,
            .type = @"type",
            .is_variadic = is_variadic,
        };
    }

    fn get_precedence(self: *Parser, token_type: TokenType) u8 {
        _ = self;

        return switch (token_type) {
            .Assign => 1,
            .Or => 2,
            .And => 3,
            .Equals, .NotEquals => 4,
            .LessThan, .GreaterThan, .LessEqual, .GreaterEqual, .Is => 5,
            .Plus, .Minus => 6,
            .Asterisk, .Divide, .Modulo => 7,
            .Dot => 8,
            .Increment, .Decrement => 9,
            .As => 10,
            else => 0,
        };
    }
};
