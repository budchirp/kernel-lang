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
const Visibility = node_zig.Visibility;

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
            try self.check_statement(statement);
        }
    }

    fn check_statement(self: *Checker, node: *Node) CheckerError!void {
        switch (node.*) {
            .VariableStatement => |*v| try self.check_variable_statement(v),
            .FunctionStatement => |*f| _ = try self.check_function_statement(f),
            .ReturnStatement => |*r| try self.check_return_statement(r),
            .IfExpression => |*i| _ = try self.check_if_expression(i),
            .WhileStatement => |*w| try self.check_while_statement(w),
            .ForStatement => |*f| try self.check_for_statement(f),
            .BlockStatement => |*b| try self.check_block_statement(b),
            .StructStatement => |*s| try self.check_struct_statement(s),
            .ExternStatement => |*e| try self.check_extern_statement(e),
            .ExpressionStatement => |*e| _ = try self.check_expression(e.expression),
            else => {
                self.context.logger.err(ErrorKind.TypeError, node.position(), "unknown statement");
                return error.TypeError;
            },
        }
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
            .TypeExpression => |*t| {
                return self.resolve_type(t.value);
            },
            .BlockStatement => |*b| {
                try self.check_block_statement(b);

                return Type.void();
            },
            else => {
                self.context.logger.err(ErrorKind.TypeError, node.position(), "unknown expression");
                return error.TypeError;
            },
        };
    }

    fn check_variable_statement(self: *Checker, statement: *node_zig.VariableStatement) CheckerError!void {
        var @"type": Type = Type.unknown();

        if (statement.type) |expression| {
            @"type" = expression.value;
        }

        if (statement.initializer) |initializer| {
            const init_type = try self.check_expression(initializer);

            if (@"type".is_unknown()) {
                @"type" = init_type;
            } else {
                if (!self.is_compatible(@"type", init_type)) {
                    self.context.logger.err(ErrorKind.TypeError, statement.position, "type mismatch in variable declaration");
                    return error.TypeError;
                }

                switch (@"type") {
                    .Array => |array| {
                        if (array.size == null) {
                            @"type".Array.size = init_type.Array.size;
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
    }

    fn check_function_proto(self: *Checker, statement: *node_zig.FunctionProto) CheckerError!FunctionSymbol {
        const env = self.program.env;
        const current_scope = env.current_scope;

        var generics = std.ArrayListUnmanaged(type_zig.GenericParameterType){};
        for (statement.generics.items) |generic| {
            const constraint: ?*const Type = if (generic.constraint) |constraint|
                try memory.create(self.allocator, Type, try self.resolve_type(constraint.value))
            else
                null;

            try generics.append(self.allocator, type_zig.GenericParameterType{
                .name = generic.name.value,
                .constraint = constraint,
            });

            try current_scope.add_type(TypeSymbol{
                .name = generic.name.value,
                .visibility = Visibility.Private,
                .type = Type{ .Named = .{ .name = generic.name.value } },
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

            const resolved_type = try self.resolve_type_with_generics(parameter.type.value, statement.generics.items);

            try parameters.append(self.allocator, type_zig.FunctionParameterType{
                .name = parameter.name.value,
                .type = try memory.create(self.allocator, Type, resolved_type),
            });
        }

        const return_type = try memory.create(self.allocator, Type, try self.resolve_type_with_generics(statement.return_type.value, statement.generics.items));

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

        try self.check_block_statement(&statement.body);

        env.set_current_scope(scope.parent);

        return symbol;
    }

    fn check_return_statement(self: *Checker, statement: *node_zig.ReturnStatement) CheckerError!void {
        const return_type = try self.check_expression(@constCast(statement.value));

        const env = self.program.env;
        const current_scope = env.current_scope;

        if (current_scope.type != .Function) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "return statement outside of function");
            return error.TypeError;
        }

        if (current_scope.function) |function| {
            if (!self.is_compatible(return_type, function.type.return_type.*)) {
                self.context.logger.err(ErrorKind.TypeError, statement.position, "return type does not match function return type");
                return error.TypeError;
            }
        }
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

            if (!self.is_compatible(consequence_type, alternative_type)) {
                self.context.logger.err(ErrorKind.TypeError, expression.position, "if branches have incompatible types");
                return error.TypeError;
            }

            expression.type = consequence_type;

            return consequence_type;
        }

        expression.type = Type{ .Void = {} };

        return expression.type;
    }

    fn check_while_statement(self: *Checker, statement: *node_zig.WhileStatement) CheckerError!void {
        const condition_type = try self.check_expression(statement.condition);
        if (condition_type != .Boolean) {
            self.context.logger.err(ErrorKind.TypeError, statement.position, "expected boolean type");
            return error.TypeError;
        }

        try self.check_block_statement(&statement.body);
    }

    fn check_for_statement(self: *Checker, statement: *node_zig.ForStatement) CheckerError!void {
        if (statement.init) |initializer| {
            try self.check_statement(initializer);
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

        try self.check_block_statement(&statement.body);
    }

    fn check_block_statement(self: *Checker, block: *node_zig.BlockStatement) CheckerError!void {
        const env = self.program.env;

        const scope = block.scope;

        env.set_current_scope(scope);

        for (block.statements.items) |*statement| {
            try self.check_statement(statement);
        }

        env.set_current_scope(scope.parent);
    }

    fn check_struct_statement(self: *Checker, statement: *node_zig.StructStatement) CheckerError!void {
        const env = self.program.env;
        const current_scope = env.current_scope;

        var generics = std.ArrayListUnmanaged(type_zig.GenericParameterType){};
        for (statement.generics.items) |generic| {
            const constraint: ?*const Type = if (generic.constraint) |constraint|
                try memory.create(self.allocator, Type, try self.resolve_type(constraint.value))
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
            const field_type = try self.resolve_type(field.type.value);

            try fields.append(self.allocator, type_zig.StructFieldType{
                .name = field.name.value,
                .type = try memory.create(self.allocator, Type, field_type),
            });
        }

        const owned_generics = try generics.toOwnedSlice(self.allocator);
        const owned_fields = try fields.toOwnedSlice(self.allocator);

        try current_scope.add_type(TypeSymbol{
            .name = statement.name.value,
            .visibility = statement.visibility,
            .type = Type{ .Struct = type_zig.StructType{
                .generics = owned_generics,
                .fields = owned_fields,
            } },
        });
    }

    fn check_array_literal(self: *Checker, literal: *node_zig.ArrayLiteral) CheckerError!Type {
        const array_type = try self.check_expression(&literal.elements.items[0]);

        for (literal.elements.items[1..]) |*element| {
            const element_type = try self.check_expression(element);

            if (!self.is_compatible(array_type, element_type)) {
                self.context.logger.err(ErrorKind.TypeError, literal.position, "array element type mismatch");
                return error.TypeError;
            }
        }

        literal.type = Type{ .Array = type_zig.ArrayType{ .child = try memory.create(self.allocator, Type, array_type), .size = literal.elements.items.len } };

        return literal.type;
    }

    fn check_extern_statement(self: *Checker, statement: *node_zig.ExternStatement) CheckerError!void {
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
                if (!self.is_numeric(left_type) or !self.is_numeric(right_type)) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "expected numeric types");
                    return error.TypeError;
                }

                if (!self.is_compatible(left_type, right_type)) {
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
                if (!self.is_compatible(left_type, right_type)) {
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
                if (!self.is_numeric(operand_type)) {
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
            .Struct => |@"struct"| blk: {
                for (@"struct".fields) |field| {
                    if (std.mem.eql(u8, field.name, expression.property.value)) {
                        break :blk field.type.*;
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

            var substitutions = std.StringHashMap(Type).init(self.allocator);
            defer substitutions.deinit();

            if (expression.generics.items.len > 0 and func.generics.len > 0) {
                if (expression.generics.items.len != func.generics.len) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "generic argument count mismatch");
                    return error.TypeError;
                }

                for (func.generics, expression.generics.items) |generic, *argument| {
                    const resolved = try self.resolve_type(argument.type.value);

                    if (generic.constraint) |constraint| {
                        if (!self.is_compatible(resolved, constraint.*)) {
                            self.context.logger.err(ErrorKind.TypeError, expression.position, "type does not satisfy constraint");
                            return error.TypeError;
                        }
                    }

                    try substitutions.put(generic.name, resolved);
                }
            }

            for (0..required_parameters) |i| {
                const parameter_type = try self.substitute_type(func.parameters[i].type.*, substitutions);

                if (!self.is_compatible(parameter_type, arguments.items[i])) {
                    self.context.logger.err(ErrorKind.TypeError, expression.position, "argument type mismatch");
                    return error.TypeError;
                }
            }

            expression.callee.Identifier.type = Type{ .Function = func };
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
        var best_match: ?type_zig.FunctionType = null;
        var best_score: usize = 0;

        for (symbols) |symbol| {
            if (symbol.type.parameters.len != arguments.len) continue;
            if (symbol.type.generics.len != generics.len) continue;

            var matches = true;
            var exact_matches: usize = 0;

            for (symbol.type.parameters, arguments) |param, arg_type| {
                if (!self.is_compatible(param.type.*, arg_type)) {
                    matches = false;
                    break;
                }
                if (self.types_equal(param.type.*, arg_type)) {
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
        const struct_type = try self.check_expression(literal.@"struct");

        switch (struct_type) {
            .Struct => |@"struct"| {
                for (literal.fields.items) |field| {
                    const expression_type = try self.check_expression(field.value);

                    var found = false;

                    for (@"struct".fields) |field_type| {
                        if (std.mem.eql(u8, field_type.name, field.name.value)) {
                            found = true;

                            if (!self.is_compatible(field_type.type.*, expression_type)) {
                                self.context.logger.err(ErrorKind.TypeError, literal.position, "field type mismatch");
                                return error.TypeError;
                            }

                            break;
                        }
                    }

                    if (!found) {
                        self.context.logger.err(ErrorKind.TypeError, literal.position, "unknown field");
                        return error.TypeError;
                    }
                }
            },
            else => {
                self.context.logger.err(ErrorKind.TypeError, literal.position, "expected struct type");
                return error.TypeError;
            },
        }

        literal.type = struct_type;

        return literal.type;
    }

    fn resolve_type(self: *Checker, @"type": Type) CheckerError!Type {
        return self.resolve_type_with_generics(@"type", &[_]node_zig.GenericParameter{});
    }

    fn resolve_type_with_generics(self: *Checker, @"type": Type, generics: []const node_zig.GenericParameter) CheckerError!Type {
        return switch (@"type") {
            .Named => |named| blk: {
                for (generics) |generic| {
                    if (std.mem.eql(u8, generic.name.value, named.name)) {
                        break :blk @"type";
                    }
                }

                if (self.program.env.current_scope.lookup_type(named.name)) |symbol| {
                    break :blk symbol.type;
                }

                break :blk Type.unknown();
            },
            .Struct => |@"struct"| blk: {
                _ = @"struct";
                break :blk @"type";
            },
            .Pointer => |pointer| blk: {
                const child = try self.resolve_type_with_generics(pointer.child.*, generics);
                break :blk Type{ .Pointer = type_zig.PointerType{ .child = try memory.create(self.allocator, Type, child) } };
            },
            .Array => |array| blk: {
                const child = try self.resolve_type_with_generics(array.child.*, generics);
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

    fn is_compatible(self: *Checker, expected: Type, actual: Type) bool {
        if (expected.is_unknown() or actual.is_unknown()) {
            return true;
        }

        if (@as(std.meta.Tag(Type), expected) != @as(std.meta.Tag(Type), actual)) {
            if (expected == .Named or actual == .Named) {
                if (expected == .Integer and actual == .Integer) return true;
                if (expected == .Float and actual == .Float) return true;
                if (expected == .Named and actual == .Named) return true;
                if (expected == .Named and actual == .Integer) return true;
                if (expected == .Integer and actual == .Named) return true;
                if (expected == .Named and actual == .Float) return true;
                if (expected == .Float and actual == .Named) return true;

                return false;
            }
            return false;
        }

        return switch (expected) {
            .Integer => |expected_int| blk: {
                const actual_int = actual.Integer;
                if (expected_int.is_unsigned != actual_int.is_unsigned) break :blk false;

                break :blk actual_int.size <= expected_int.size;
            },
            .Float => |expected_float| blk: {
                const actual_float = actual.Float;

                break :blk actual_float.size <= expected_float.size;
            },
            .String => true,
            .Boolean => true,
            .Void => true,
            .Pointer => |pointer| {
                if (actual != .Pointer) return false;

                if (pointer.child.* == .Void) return true;

                return self.is_compatible(pointer.child.*, actual.Pointer.child.*);
            },
            .Array => |array| {
                if (actual != .Array) return false;

                return self.is_compatible(array.child.*, actual.Array.child.*);
            },
            .Struct => |@"struct"| {
                if (actual != .Struct) return false;

                if (@"struct".fields.len != actual.Struct.fields.len) return false;

                for (@"struct".fields, 0..) |field, i| {
                    if (!std.mem.eql(u8, field.name, actual.Struct.fields[i].name)) return false;
                    if (!self.is_compatible(field.type.*, actual.Struct.fields[i].type.*)) return false;
                }

                return true;
            },
            .Function => |function| {
                if (actual != .Function) return false;

                if (!self.is_compatible(function.return_type.*, actual.Function.return_type.*)) return false;

                if (function.parameters.len != actual.Function.parameters.len) return false;

                for (function.parameters, 0..) |p, i| {
                    if (!self.is_compatible(p.type.*, actual.Function.parameters[i].type.*)) return false;
                }

                return true;
            },
            .Unknown => true,
            .Named => true,
        };
    }

    fn is_numeric(self: *Checker, @"type": Type) bool {
        _ = self;
        return @"type" == .Integer or @"type" == .Float or @"type".is_unknown() or @"type" == .Named;
    }

    fn types_equal(self: *Checker, a: Type, b: Type) bool {
        _ = self;
        if (@as(std.meta.Tag(Type), a) != @as(std.meta.Tag(Type), b)) return false;

        return switch (a) {
            .Integer => |ai| ai.size == b.Integer.size and ai.is_unsigned == b.Integer.is_unsigned,
            .Float => |af| af.size == b.Float.size,
            else => true,
        };
    }
};
