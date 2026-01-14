const std = @import("std");

const node_zig = @import("../ast/node.zig");
const program_zig = @import("../ast/program.zig");
const type_zig = @import("../types/type.zig");
const symbol_zig = @import("../symbol/symbol.zig");
const logger_zig = @import("../logger/logger.zig");
const source_zig = @import("../lexer/source.zig");
const position_zig = @import("../lexer/position.zig");
const compiler_zig = @import("../compiler/context.zig");
const scope_zig = @import("../symbol/scope.zig");
const memory_zig = @import("../utils/memory.zig");

const CompilerContext = compiler_zig.CompilerContext;
const Node = node_zig.Node;
const Program = program_zig.Program;
const Type = type_zig.Type;
const Symbol = symbol_zig.Symbol;
const TypeSymbol = symbol_zig.TypeSymbol;
const FunctionSymbol = symbol_zig.FunctionSymbol;
const VariableSymbol = symbol_zig.VariableSymbol;
const Scope = scope_zig.Scope;
const Logger = logger_zig.Logger;
const ErrorKind = logger_zig.error_kind_zig.ErrorKind;
const Source = source_zig.Source;
const Position = position_zig.Position;
const Visibility = symbol_zig.Visibility;

const memory = memory_zig;

pub const CheckerError = error{
    TypeError,
    UndefinedSymbol,
    RedefinedSymbol,
    DuplicateFunction,
    OutOfMemory,
};

pub const Checker = struct {
    allocator: std.mem.Allocator,

    context: CompilerContext,
    program: *Program,

    pub fn init(allocator: std.mem.Allocator, context: CompilerContext, program: *Program) !Checker {
        return Checker{
            .allocator = allocator,
            .context = context,
            .program = program,
        };
    }

    pub fn deinit(self: *Checker) void {
        _ = self;
    }

    pub fn check(self: *Checker) CheckerError!void {
        for (self.program.statements.items) |*statement| {
            _ = try self.check_statement(statement);
        }
    }

    fn check_statement(self: *Checker, node: *Node) CheckerError!Type {
        return switch (node.*) {
            .VariableStatement => |*v| try self.check_variable_statement(v),
            .FunctionStatement => |*f| blk: {
                _ = try self.check_function_statement(f);
                break :blk Type.void();
            },
            .ReturnStatement => |*r| try self.check_return_statement(r),
            .WhileStatement => |*w| try self.check_while_statement(w),
            .ForStatement => |*f| try self.check_for_statement(f),
            .BlockStatement => |*b| try self.check_block_statement(b),
            .StructStatement => |*s| try self.check_struct_statement(s),
            .ExternStatement => |*e| try self.check_extern_statement(e),
            .ExpressionStatement => |*e| blk: {
                const @"type" = try self.check_expression(e.expression);

                e.type = @"type";

                break :blk e.type;
            },
            else => {
                self.context.logger.err(ErrorKind.TypeError, node.position(), "unknown statement");
                return error.TypeError;
            },
        };
    }

    fn check_expression(self: *Checker, node: *Node) CheckerError!Type {
        return switch (node.*) {
            .Identifier => |*i| self.check_identifier(i),
            .NumberLiteral => |*n| self.check_number_literal(n),
            .FloatLiteral => |*f| self.check_float_literal(f),
            .StringLiteral => |*s| self.check_string_literal(s),
            .BooleanLiteral => |*b| self.check_boolean_literal(b),
            .BinaryExpression => |*b| self.check_binary_expression(b),
            .UnaryExpression => |*u| self.check_unary_expression(u),
            .CallExpression => |*c| self.check_call_expression(c),
            .MemberExpression => |*m| self.check_member_expression(m),
            .IndexExpression => |*i| self.check_index_expression(i),
            .IfExpression => |*i| self.check_if_expression(i),
            .StructLiteral => |*s| self.check_struct_literal(s),
            .ArrayLiteral => |*a| self.check_array_literal(a),
            .TypeExpression => |*t| self.check_type_expression(t),
            .BlockStatement => |*b| self.check_block_statement(b),
            else => {
                self.context.logger.err(ErrorKind.TypeError, node.position(), "unknown expression");
                return error.TypeError;
            },
        };
    }

    fn check_variable_statement(self: *Checker, statement: *node_zig.VariableStatement) CheckerError!Type {
        var @"type": Type = Type.unknown();

        if (statement.type) |*type_expression| {
            @"type" = try self.check_type_expression(type_expression);
        }

        if (statement.initializer) |initializer_expression| {
            const initializer_type = try self.check_expression(initializer_expression);

            if (@"type".is_unknown()) {
                @"type" = initializer_type;
            } else {
                if (!Type.is_compatible(@"type", initializer_type)) {
                    self.context.logger.err(ErrorKind.TypeError, statement.position, "type mismatch in variable declaration");
                    return error.TypeError;
                }

                switch (@"type") {
                    .Array => |array| {
                        if (array.size == null) {
                            @"type".Array.size = initializer_type.Array.size;
                        }
                    },
                    else => {},
                }
            }
        }

        if (@"type".is_unknown()) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "cannot infer type for variable");
            return error.TypeError;
        }

        const env = self.program.env;
        const current_scope = env.current_scope;

        if (current_scope.lookup_variable(statement.name.value) != null) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "variable already defined");
            return error.RedefinedSymbol;
        }

        try current_scope.add_variable(VariableSymbol{
            .name = statement.name.value,
            .visibility = statement.visibility,
            .type = @"type",
            .is_mutable = statement.is_mut,
        });

        return Type.void();
    }

    fn check_function_proto(self: *Checker, statement: *node_zig.FunctionProto) CheckerError!FunctionSymbol {
        const env = self.program.env;
        const current_scope = env.current_scope;

        var generics = std.ArrayListUnmanaged(type_zig.GenericParameterType){};
        for (statement.generics.items) |generic| {
            const constraint: ?*const Type = if (generic.constraint) |*constraint|
                try memory.create(self.allocator, Type, try self.check_type_expression(@constCast(constraint)))
            else
                null;

            try generics.append(self.allocator, type_zig.GenericParameterType{
                .name = generic.name.value,
                .constraint = constraint,
            });

            try current_scope.add_type(TypeSymbol{
                .name = generic.name.value,
                .visibility = Visibility.Private,
                .type = Type{
                    .Named = .{
                        .name = generic.name.value,
                    },
                },
            });
        }

        var parameters = std.ArrayListUnmanaged(type_zig.FunctionParameterType){};

        var is_variadic = false;
        for (statement.parameters.items, 0..) |parameter, i| {
            if (parameter.is_variadic) {
                is_variadic = true;

                if (i != statement.parameters.items.len - 1) {
                    self.context.logger.err(ErrorKind.TypeError, parameter.position, "variadic parameter must be last");
                    return error.TypeError;
                }

                if (parameter.type.value != .Array) {
                    self.context.logger.err(ErrorKind.TypeError, parameter.position, "variadic parameter must be array type");
                    return error.TypeError;
                }
            }

            const resolved_type = try self.check_type_expression_with_generics(@constCast(&parameter.type), generics.items);

            try parameters.append(self.allocator, type_zig.FunctionParameterType{
                .name = parameter.name.value,
                .type = try memory.create(self.allocator, Type, resolved_type),
            });
        }

        const return_type = try memory.create(self.allocator, Type, try self.check_type_expression_with_generics(@constCast(&statement.return_type), generics.items));

        const owned_parameters = try parameters.toOwnedSlice(self.allocator);
        const owned_generics = try generics.toOwnedSlice(self.allocator);

        const symbol = FunctionSymbol{
            .name = statement.name.value,
            .visibility = Visibility.Private,
            .type = type_zig.FunctionType{
                .parameters = owned_parameters,
                .generics = owned_generics,
                .return_type = return_type,
                .is_variadic = is_variadic,
                .is_operator = false,
            },
        };

        current_scope.add_function(symbol) catch |err| switch (err) {
            error.DuplicateFunction => {
                self.context.logger.err(ErrorKind.TypeError, statement.name.position, "duplicate function with same signature");
                return error.TypeError;
            },
            else => return err,
        };

        return symbol;
    }

    fn check_function_statement(self: *Checker, statement: *node_zig.FunctionStatement) CheckerError!FunctionSymbol {
        const symbol = try self.check_function_proto(&statement.proto);

        const env = self.program.env;

        const scope = statement.body.scope;

        env.set_current_scope(scope);
        defer env.set_current_scope(scope.parent);

        scope.type = .Function;
        scope.function = symbol;

        for (statement.proto.parameters.items) |parameter| {
            try scope.add_variable(VariableSymbol{
                .name = parameter.name.value,
                .visibility = Visibility.Private,
                .type = parameter.type.value,
                .is_mutable = false,
            });
        }

        _ = try self.check_block_statement(&statement.body);

        return symbol;
    }

    fn check_return_statement(self: *Checker, statement: *node_zig.ReturnStatement) CheckerError!Type {
        const return_type = try self.check_expression(@constCast(statement.value));

        const env = self.program.env;
        const current_scope = env.current_scope;

        if (!current_scope.is_function()) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "return statement outside of function");
            return error.TypeError;
        }

        if (current_scope.function) |function| {
            if (!Type.is_compatible(return_type, function.type.return_type.*)) {
                self.context.logger.err(ErrorKind.TypeError, statement.position, "return type does not match function return type");
                return error.TypeError;
            }
        }

        statement.type = return_type;

        return statement.type;
    }

    fn check_if_expression(self: *Checker, expression: *node_zig.IfExpression) CheckerError!Type {
        const condition_type = try self.check_expression(expression.condition);
        if (condition_type != .Boolean) {
            self.context.logger.err(ErrorKind.TypeError, expression.position, "expected boolean type");
            return error.TypeError;
        }

        const consequence_type = try self.check_expression(expression.consequence);

        if (expression.alternative) |alternative| {
            const alternative_type = try self.check_expression(alternative);

            if (!Type.is_compatible(consequence_type, alternative_type)) {
                self.context.logger.err(ErrorKind.TypeError, expression.position, "if branches have incompatible types");
                return error.TypeError;
            }

            expression.type = consequence_type;

            return consequence_type;
        }

        expression.type = Type{ .Void = {} };

        return expression.type;
    }

    fn check_while_statement(self: *Checker, statement: *node_zig.WhileStatement) CheckerError!Type {
        const condition_type = try self.check_expression(statement.condition);
        if (condition_type != .Boolean) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "expected boolean type");
            return error.TypeError;
        }

        _ = try self.check_block_statement(&statement.body);

        return Type.void();
    }

    fn check_for_statement(self: *Checker, statement: *node_zig.ForStatement) CheckerError!Type {
        if (statement.init) |initializer| {
            _ = try self.check_expression(initializer);
        }

        if (statement.condition) |condition| {
            const condition_type = try self.check_expression(condition);
            if (condition_type != .Boolean) {
                self.context.logger.err(ErrorKind.TypeError, statement.position, "expected boolean type");
                return error.TypeError;
            }
        }

        if (statement.update) |update| {
            _ = try self.check_expression(update);
        }

        _ = try self.check_block_statement(&statement.body);

        return Type.void();
    }

    fn check_block_statement(self: *Checker, block: *node_zig.BlockStatement) CheckerError!Type {
        const env = self.program.env;

        const scope = block.scope;

        env.set_current_scope(scope);

        var last: Type = undefined;
        for (block.statements.items) |*statement| {
            last = try self.check_statement(statement);
        }

        block.type = last;

        env.set_current_scope(scope.parent);

        return last;
    }

    fn check_struct_statement(self: *Checker, statement: *node_zig.StructStatement) CheckerError!Type {
        const env = self.program.env;
        const current_scope = env.current_scope;

        var generics = std.ArrayListUnmanaged(type_zig.GenericParameterType){};
        for (statement.generics.items) |generic| {
            const constraint: ?*const Type = if (generic.constraint) |*constraint|
                try memory.create(self.allocator, Type, try self.check_type_expression(@constCast(constraint)))
            else
                null;

            try generics.append(self.allocator, type_zig.GenericParameterType{
                .name = generic.name.value,
                .constraint = constraint,
            });
        }

        var fields = std.ArrayListUnmanaged(type_zig.StructFieldType){};

        if (statement.parent) |parent| {
            switch (parent.value) {
                .Struct => |parent_struct| {
                    for (parent_struct.fields) |field| {
                        try fields.append(self.allocator, field);
                    }
                },
                else => {
                    self.context.logger.err(ErrorKind.TypeError, statement.position, "parent must be a struct");
                    return error.TypeError;
                },
            }
        }

        for (statement.fields.items) |*field| {
            const field_type = try self.check_type_expression_with_generics(@constCast(&field.type), generics.items);

            try fields.append(self.allocator, type_zig.StructFieldType{
                .name = field.name.value,
                .type = try memory.create(self.allocator, Type, field_type),
            });
        }

        const owned_generics = try generics.toOwnedSlice(self.allocator);
        const owned_fields = try fields.toOwnedSlice(self.allocator);

        const struct_type = type_zig.StructType{
            .name = statement.name.value,
            .generics = owned_generics,
            .fields = owned_fields,
        };

        statement.type = struct_type;

        try current_scope.add_type(TypeSymbol{
            .name = statement.name.value,
            .visibility = statement.visibility,
            .type = Type{ .Struct = struct_type },
        });

        return Type.void();
    }

    fn check_array_literal(self: *Checker, literal: *node_zig.ArrayLiteral) CheckerError!Type {
        const array_type = try self.check_expression(&literal.elements.items[0]);

        for (literal.elements.items[1..]) |*element| {
            const element_type = try self.check_expression(element);

            if (!Type.is_compatible(array_type, element_type)) {
                self.context.logger.err(ErrorKind.TypeError, literal.position, "array element type mismatch");
                return error.TypeError;
            }
        }

        literal.type = Type{ .Array = type_zig.ArrayType{ .child = try memory.create(self.allocator, Type, array_type), .size = literal.elements.items.len } };

        return literal.type;
    }

    fn check_extern_statement(self: *Checker, statement: *node_zig.ExternStatement) CheckerError!Type {
        const env = self.program.env;
        const current_scope = env.current_scope;

        switch (statement.*) {
            .Function => |*function| {
                _ = try self.check_function_proto(&function.proto);
            },
            .Variable => |variable| {
                try current_scope.add_variable(VariableSymbol{
                    .name = variable.name.value,
                    .visibility = variable.visibility,
                    .type = variable.type.value,
                    .is_mutable = false,
                });
            },
        }

        return Type.void();
    }

    fn check_identifier(self: *Checker, identifier: *node_zig.Identifier) CheckerError!Type {
        const env = self.program.env;
        const current_scope = env.current_scope;

        if (current_scope.lookup_variable(identifier.value)) |symbol| {
            identifier.type = symbol.type;
            return symbol.type;
        }

        if (current_scope.lookup_first_function(identifier.value)) |function| {
            const function_type = Type{ .Function = function.type };
            identifier.type = function_type;

            return function_type;
        }

        if (current_scope.lookup_type(identifier.value)) |@"type"| {
            identifier.type = @"type".type;
            return @"type".type;
        }

        std.debug.print("undefined symbol: {s}\n", .{identifier.value});
        self.context.logger.err(ErrorKind.TypeError, identifier.position, "undefined symbol");
        return error.UndefinedSymbol;
    }

    fn check_number_literal(self: *Checker, literal: *node_zig.NumberLiteral) CheckerError!Type {
        _ = self;

        literal.type = Type{ .Integer = .{ .size = 32, .is_unsigned = false } };
        return literal.type;
    }

    fn check_float_literal(self: *Checker, literal: *node_zig.FloatLiteral) CheckerError!Type {
        _ = self;

        literal.type = Type{ .Float = .{ .size = 64 } };
        return literal.type;
    }

    fn check_string_literal(self: *Checker, literal: *node_zig.StringLiteral) CheckerError!Type {
        _ = self;

        literal.type = Type{ .String = {} };
        return literal.type;
    }

    fn check_boolean_literal(self: *Checker, literal: *node_zig.BooleanLiteral) CheckerError!Type {
        _ = self;

        literal.type = Type{ .Boolean = {} };
        return literal.type;
    }

    fn check_binary_expression(self: *Checker, expression: *node_zig.BinaryExpression) CheckerError!Type {
        const left_type = try self.check_expression(expression.left);
        const right_type = try self.check_expression(expression.right);

        const result_type = switch (expression.operator.type) {
            .Plus, .Minus, .Asterisk, .Divide, .Modulo => blk: {
                if (!left_type.is_numeric() or !right_type.is_numeric()) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected numeric types");
                    return error.TypeError;
                }

                if (!Type.is_compatible(left_type, right_type)) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "incompatible types in binary expression");
                    return error.TypeError;
                }

                break :blk left_type;
            },
            .Equals, .NotEquals, .LessThan, .GreaterThan, .LessEqual, .GreaterEqual, .Is => Type{ .Boolean = {} },
            .And, .Or => blk: {
                if (left_type != .Boolean or right_type != .Boolean) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected boolean types");
                    return error.TypeError;
                }

                break :blk Type{ .Boolean = {} };
            },
            .Assign => blk: {
                if (!Type.is_compatible(left_type, right_type)) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "assignment requires compatible types");
                    return error.TypeError;
                }

                break :blk Type.unknown();
            },
            .As => right_type,
            else => Type.unknown(),
        };

        expression.type = result_type;

        return result_type;
    }

    fn check_unary_expression(self: *Checker, expression: *node_zig.UnaryExpression) CheckerError!Type {
        const operand_type = try self.check_expression(expression.operand);

        const result_type = switch (expression.operator.type) {
            .Minus => blk: {
                if (!operand_type.is_numeric()) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected numeric type");
                    return error.TypeError;
                }

                break :blk operand_type;
            },
            .Not => switch (operand_type) {
                .Boolean => operand_type,
                else => {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected boolean type");
                    return error.TypeError;
                },
            },
            .Asterisk => switch (operand_type) {
                .Pointer => operand_type.Pointer.child.*,
                else => {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected pointer type");
                    return error.TypeError;
                },
            },
            .Ampersand => Type{ .Pointer = type_zig.PointerType{ .child = try memory.create(self.allocator, Type, operand_type) } },
            .Delete => switch (operand_type) {
                .Pointer => Type.void(),
                else => {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected pointer type");
                    return error.TypeError;
                },
            },
            else => Type.unknown(),
        };

        expression.type = result_type;

        return result_type;
    }

    fn check_member_expression(self: *Checker, expression: *node_zig.MemberExpression) CheckerError!Type {
        const object_type = try self.check_expression(expression.object);

        const property_type = switch (object_type) {
            .StructLiteral => |struct_literal| blk: {
                var substitutions = std.StringHashMap(Type).init(self.allocator);
                for (struct_literal.@"struct".*.generics, struct_literal.generics) |generic_parameter, generic_argument| {
                    try substitutions.put(generic_parameter.name, generic_argument.type.*);
                }

                for (struct_literal.@"struct".*.fields) |field| {
                    if (std.mem.eql(u8, field.name, expression.property.value)) {
                        break :blk try self.substitute_type(field.type.*, substitutions);
                    }
                }

                break :blk null;
            },
            else => null,
        };

        if (property_type) |resloved| {
            expression.type = resloved;

            return resloved;
        }

        self.context.logger.err(ErrorKind.TypeError, expression.position, "unknown member");
        return error.TypeError;
    }

    fn check_call_expression(self: *Checker, expression: *node_zig.CallExpression) CheckerError!Type {
        const env = self.program.env;
        const current_scope = env.current_scope;

        var arguments = std.ArrayListUnmanaged(Type){};
        for (expression.arguments.items) |*argument| {
            try arguments.append(self.allocator, try self.check_expression(argument.value));
        }

        var function: ?type_zig.FunctionType = null;

        if (expression.callee.* == .Identifier) {
            const name = expression.callee.Identifier.value;
            if (current_scope.lookup_function(name)) |symbols| {
                function = self.find_matching_overload(symbols, arguments.items, expression.generics.items);
                if (function) |func| {
                    expression.callee.Identifier.type = Type{ .Function = func };
                }
            }
        }

        if (function == null) {
            const callee_type = try self.check_expression(expression.callee);
            if (callee_type == .Function) {
                function = callee_type.Function;
            }
        }

        if (function) |func| {
            const required_parameters = if (func.is_variadic) func.parameters.len - 1 else func.parameters.len;
            if (func.is_variadic) {
                if (expression.arguments.items.len < required_parameters) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "too few arguments for variadic function");
                    return error.TypeError;
                }
            } else {
                if (func.parameters.len != expression.arguments.items.len) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "argument count mismatch");
                    return error.TypeError;
                }
            }

            if (expression.generics.items.len != func.generics.len) {
                self.context.logger.err(ErrorKind.TypeError, expression.position, "generic argument count mismatch");
                return error.TypeError;
            }

            var substitutions = std.StringHashMap(Type).init(self.allocator);
            defer substitutions.deinit();
            for (func.generics, expression.generics.items) |generic_parameter, *generic_argument| {
                const resolved = try self.check_type_expression(@constCast(&generic_argument.type));

                if (generic_parameter.constraint) |constraint| {
                    if (!Type.is_compatible(resolved, constraint.*)) {
                        self.context.logger.err(ErrorKind.TypeError, expression.position, "type does not satisfy constraint");
                        return error.TypeError;
                    }
                }

                try substitutions.put(generic_parameter.name, resolved);
            }

            for (0..required_parameters) |i| {
                const parameter_type = try self.substitute_type(func.parameters[i].type.*, substitutions);

                if (!Type.is_compatible(parameter_type, arguments.items[i])) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "argument type mismatch");
                    return error.TypeError;
                }
            }

            expression.type = try self.substitute_type(func.return_type.*, substitutions);

            return expression.type;
        }

        self.context.logger.err(ErrorKind.TypeError, expression.position, "no matching function overload found");
        return error.TypeError;
    }

    fn find_matching_overload(
        self: *Checker,
        symbols: []FunctionSymbol,
        arguments: []const Type,
        generics: []node_zig.GenericArgument,
    ) ?type_zig.FunctionType {
        _ = self;

        var best_match: ?type_zig.FunctionType = null;
        var best_score: usize = 0;

        for (symbols) |symbol| {
            if (symbol.type.parameters.len != arguments.len) continue;
            if (symbol.type.generics.len != generics.len) continue;

            var matches = true;
            var exact_matches: usize = 0;

            for (symbol.type.parameters, arguments) |param, arg_type| {
                if (!Type.is_compatible(param.type.*, arg_type)) {
                    matches = false;
                    break;
                }
                if (Type.equal(param.type.*, arg_type)) {
                    exact_matches += 1;
                }
            }

            if (matches) {
                if (best_match == null or exact_matches > best_score) {
                    best_match = symbol.type;
                    best_score = exact_matches;
                }
            }
        }

        return best_match;
    }

    fn check_index_expression(self: *Checker, expression: *node_zig.IndexExpression) CheckerError!Type {
        const array_type = try self.check_expression(expression.array);

        _ = try self.check_expression(expression.index);

        switch (array_type) {
            .Array => |array| {
                expression.type = array.child.*;
                return expression.type;
            },
            else => {
                self.context.logger.err(ErrorKind.TypeError, expression.position, "expected array type");
                return error.TypeError;
            },
        }
    }

    fn check_struct_literal(self: *Checker, literal: *node_zig.StructLiteral) CheckerError!Type {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const type_symbol = current_scope.lookup_type(literal.name.value);
        if (type_symbol == null) {
            self.context.logger.err(ErrorKind.TypeError, literal.position, "struct not found");
            return error.TypeError;
        }

        const struct_type = type_symbol.?.type.Struct;

        if (literal.generics.items.len != struct_type.generics.len) {
            self.context.logger.err(ErrorKind.TypeError, literal.position, "generic argument count mismatch");
            return error.TypeError;
        }

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();

        for (literal.generics.items, 0..) |*generic_argument, i| {
            const resolved = try self.check_type_expression(@constCast(&generic_argument.type));
            const name = if (generic_argument.name) |n| n.value else struct_type.generics[i].name;
            try substitutions.put(name, resolved);
        }

        var fields = std.ArrayListUnmanaged(type_zig.StructFieldType){};
        for (struct_type.fields) |field_type| {
            const substituted = try self.substitute_type(field_type.type.*, substitutions);
            try fields.append(self.allocator, type_zig.StructFieldType{
                .name = field_type.name,
                .type = try memory.create(self.allocator, Type, substituted),
            });
        }

        var generic_parameters = std.ArrayListUnmanaged(type_zig.GenericParameterType){};
        for (struct_type.generics) |generic| {
            try generic_parameters.append(self.allocator, generic);
        }

        var generic_arguments = std.ArrayListUnmanaged(type_zig.GenericArgumentType){};
        for (literal.generics.items) |generic_argument| {
            try generic_arguments.append(self.allocator, type_zig.GenericArgumentType{
                .type = try memory.create(self.allocator, Type, try self.check_type_expression(@constCast(&generic_argument.type))),
            });
        }

        const owned_generic_parameters = try generic_parameters.toOwnedSlice(self.allocator);
        const owned_generic_arguments = try generic_arguments.toOwnedSlice(self.allocator);
        const owned_fields = try fields.toOwnedSlice(self.allocator);

        for (literal.fields.items) |field| {
            const expression_type = try self.check_expression(field.value);

            var found = false;

            for (owned_fields) |field_type| {
                if (std.mem.eql(u8, field_type.name, field.name.value)) {
                    found = true;

                    if (!Type.is_compatible(field_type.type.*, expression_type)) {
                        self.context.logger.err(ErrorKind.TypeError, literal.position, "field type mismatch");
                        return error.TypeError;
                    }
                }
            }

            if (!found) {
                self.context.logger.err(ErrorKind.TypeError, literal.position, "unknown field");
                return error.TypeError;
            }
        }

        literal.type = type_zig.StructLiteralType{
            .@"struct" = try memory.create(self.allocator, type_zig.StructType, type_zig.StructType{
                .name = struct_type.name,
                .generics = owned_generic_parameters,
                .fields = owned_fields,
            }),
            .generics = owned_generic_arguments,
        };

        return Type{ .StructLiteral = literal.type.? };
    }

    fn check_type_expression(self: *Checker, expression: *node_zig.TypeExpression) CheckerError!Type {
        return self.check_type_expression_with_generics(expression, &[_]type_zig.GenericParameterType{});
    }

    fn check_type_expression_with_generics(self: *Checker, expression: *node_zig.TypeExpression, generics: []const type_zig.GenericParameterType) CheckerError!Type {
        const @"type" = try self.resolve_type(expression.value, generics);

        expression.value = @"type";

        return @"type";
    }

    fn resolve_type(self: *Checker, @"type": Type, generics: []const type_zig.GenericParameterType) CheckerError!Type {
        return switch (@"type") {
            .Named => |named| blk: {
                for (generics) |generic| {
                    if (std.mem.eql(u8, generic.name, named.name)) {
                        break :blk @"type";
                    }
                }

                const symbol = self.program.env.current_scope.lookup_type(named.name);
                if (symbol == null) {
                    break :blk Type.unknown();
                }

                return switch (symbol.?.type) {
                    .Struct => |struct_type| Type{ .StructLiteral = .{
                        .@"struct" = try memory.create(self.allocator, type_zig.StructType, struct_type),
                        .generics = named.generics,
                    } },
                    else => symbol.?.type,
                };
            },
            .Struct => |@"struct"| blk: {
                _ = @"struct";
                break :blk @"type";
            },
            .Pointer => |pointer| blk: {
                const child = try self.resolve_type(pointer.child.*, generics);
                break :blk Type{ .Pointer = type_zig.PointerType{ .child = try memory.create(self.allocator, Type, child) } };
            },
            .Array => |array| blk: {
                const child = try self.resolve_type(array.child.*, generics);
                break :blk Type{ .Array = type_zig.ArrayType{ .child = try memory.create(self.allocator, Type, child), .size = array.size } };
            },
            else => @"type",
        };
    }

    fn substitute_type(self: *Checker, @"type": Type, substitutions: std.StringHashMap(Type)) CheckerError!Type {
        return switch (@"type") {
            .Named => |named| substitutions.get(named.name) orelse @"type",
            .Pointer => |pointer| Type{ .Pointer = .{
                .child = try memory.create(self.allocator, Type, try self.substitute_type(pointer.child.*, substitutions)),
            } },
            .Array => |array| Type{ .Array = .{
                .child = try memory.create(self.allocator, Type, try self.substitute_type(array.child.*, substitutions)),
                .size = array.size,
            } },
            else => @"type",
        };
    }
};
