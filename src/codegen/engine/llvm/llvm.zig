const std = @import("std");

const c = @import("c.zig").c;

const context_zig = @import("context.zig");
const node_zig = @import("../../../ast/node.zig");
const program_zig = @import("../../../ast/program.zig");
const type_zig = @import("../../../types/type.zig");
const compiler_zig = @import("../../../compiler/context.zig");
const symbol_zig = @import("../../../symbol/symbol.zig");
const scope_zig = @import("../../../symbol/scope.zig");
const mono_zig = @import("../../monomorphization.zig");
const memory_zig = @import("../../../utils/memory.zig");
const builtin_zig = @import("../../../symbol/builtin.zig");

const Type = type_zig.Type;
const Node = node_zig.Node;
const CompilerContext = compiler_zig.CompilerContext;
const FunctionSymbol = symbol_zig.FunctionSymbol;
const Program = program_zig.Program;
const LLVMCodegenContext = context_zig.LLVMCodegenContext;
const MonomorphizationCache = mono_zig.MonomorphizationCache;

const BlockStatement = node_zig.BlockStatement;
const FunctionStatement = node_zig.FunctionStatement;
const FunctionProto = node_zig.FunctionProto;
const VariableStatement = node_zig.VariableStatement;
const ReturnStatement = node_zig.ReturnStatement;
const BinaryExpression = node_zig.BinaryExpression;
const UnaryExpression = node_zig.UnaryExpression;
const CallExpression = node_zig.CallExpression;
const Identifier = node_zig.Identifier;
const ArrayLiteral = node_zig.ArrayLiteral;
const IndexExpression = node_zig.IndexExpression;

pub const CodegenError = error{
    InvalidType,
    OutOfMemory,
    VariableNotFound,
    FunctionNotFound,
};

pub const CodegenResultContext = union(enum) {
    void: void,
    function: FunctionSymbol,
    @"struct": c.LLVMTypeRef,
};

pub const CodegenResult = struct {
    value: c.LLVMValueRef,
    context: CodegenResultContext,
};

pub const CodegenOptions = struct {
    load: bool = true,
    type: ?Type = null,
};

pub const LLVMCodegen = struct {
    allocator: std.mem.Allocator,

    context: CompilerContext,
    llvm_context: LLVMCodegenContext,

    program: *Program,

    variables: std.StringHashMap(c.LLVMValueRef),

    mono_cache: MonomorphizationCache,
    substitutions: ?std.StringHashMap(Type) = null,

    pub fn to_llvm_type(self: *LLVMCodegen, @"type": Type) CodegenError!c.LLVMTypeRef {
        switch (self.resolve_type(@"type")) {
            .Integer => |integer| {
                return switch (integer.size) {
                    8 => c.LLVMInt8TypeInContext(self.llvm_context.llvm),
                    16 => c.LLVMInt16TypeInContext(self.llvm_context.llvm),
                    32 => c.LLVMInt32TypeInContext(self.llvm_context.llvm),
                    64 => c.LLVMInt64TypeInContext(self.llvm_context.llvm),
                    else => error.InvalidType,
                };
            },
            .Float => |float| {
                return switch (float.size) {
                    32 => c.LLVMFloatTypeInContext(self.llvm_context.llvm),
                    64 => c.LLVMDoubleTypeInContext(self.llvm_context.llvm),
                    else => error.InvalidType,
                };
            },
            .Boolean => return c.LLVMInt1TypeInContext(self.llvm_context.llvm),
            .String => return c.LLVMPointerType(c.LLVMInt8TypeInContext(self.llvm_context.llvm), 0),
            .Array => |array| {
                if (array.size) |size| {
                    return c.LLVMArrayType(try self.to_llvm_type(array.child.*), @intCast(size));
                } else {
                    return c.LLVMPointerType(try self.to_llvm_type(array.child.*), 0);
                }
            },
            .Pointer => |pointer| return c.LLVMPointerType(try self.to_llvm_type(pointer.child.*), 0),
            .Function => |function| {
                var parameters = std.ArrayListUnmanaged(c.LLVMTypeRef){};
                defer parameters.deinit(self.allocator);

                const param_count = if (function.is_variadic and function.parameters.len > 0) function.parameters.len - 1 else function.parameters.len;
                for (function.parameters[0..param_count]) |parameter| {
                    try parameters.append(self.allocator, try self.to_llvm_type(parameter.type.*));
                }

                const owned_parameters = try parameters.toOwnedSlice(self.allocator);
                defer self.allocator.free(owned_parameters);

                return c.LLVMFunctionType(try self.to_llvm_type(function.return_type.*), owned_parameters.ptr, @intCast(owned_parameters.len), @intFromBool(function.is_variadic));
            },
            .Struct => |@"struct"| {
                const name_z = try self.allocator.dupeZ(u8, @"struct".name);
                defer self.allocator.free(name_z);

                const llvm_struct_type = c.LLVMGetTypeByName(self.llvm_context.module, name_z.ptr);
                if (llvm_struct_type == null) {
                    const result = try self.generate_struct_definition(@"struct", @"struct".name);

                    return result.context.@"struct";
                }

                return llvm_struct_type;
            },
            .StructLiteral => |struct_literal| {
                var generic_arguments = std.ArrayListUnmanaged(Type){};
                defer generic_arguments.deinit(self.allocator);
                for (struct_literal.generics) |generic_argument| try generic_arguments.append(self.allocator, generic_argument.type.*);

                const cache_result = try self.mono_cache.get_or_create(struct_literal.@"struct".name, generic_arguments.items);
                if (!cache_result.is_generated) {
                    if (self.mono_cache.get_struct(struct_literal.@"struct".name)) |statement| {
                        _ = try self.generate_monomorphized_struct_statement(statement, cache_result.name, generic_arguments.items);
                        try self.mono_cache.mark_generated(cache_result.name);
                    }
                }

                var substitutions = std.StringHashMap(Type).init(self.allocator);
                defer substitutions.deinit();
                for (struct_literal.@"struct".generics, 0..) |generic_parameter, i| {
                    if (i < generic_arguments.items.len) {
                        try substitutions.put(generic_parameter.name, generic_arguments.items[i]);
                    }
                }

                const old_substitutions = self.substitutions;
                self.substitutions = substitutions;
                defer self.substitutions = old_substitutions;

                const name_z = try self.allocator.dupeZ(u8, cache_result.name);
                defer self.allocator.free(name_z);

                const llvm_struct_type = c.LLVMGetTypeByName(self.llvm_context.module, name_z.ptr);

                return llvm_struct_type;
            },
            .Named => |named| {
                const env = self.program.env;
                const current_scope = env.current_scope;

                if (current_scope.lookup_type(named.name)) |symbol| {
                    return self.to_llvm_type(self.resolve_type(symbol.type));
                }

                return try self.to_llvm_type(self.resolve_type(Type{ .Named = named }));
            },
            .Void => return c.LLVMVoidTypeInContext(self.llvm_context.llvm),
            else => {
                return error.InvalidType;
            },
        }
    }

    pub fn init(allocator: std.mem.Allocator, name: [:0]const u8, context: CompilerContext, program: *Program) !LLVMCodegen {
        const llvm_context = try LLVMCodegenContext.init(name);

        return LLVMCodegen{
            .allocator = allocator,
            .context = context,
            .llvm_context = llvm_context,
            .program = program,
            .variables = std.StringHashMap(c.LLVMValueRef).init(allocator),
            .mono_cache = MonomorphizationCache.init(allocator),
        };
    }

    pub fn deinit(self: *LLVMCodegen) void {
        var iter = self.variables.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();

        self.mono_cache.deinit();
    }

    pub fn allocate(self: *LLVMCodegen) !void {
        try self.llvm_context.allocate();
    }

    pub fn deallocate(self: *LLVMCodegen) void {
        self.llvm_context.deallocate();
    }

    pub fn dump(self: *LLVMCodegen) void {
        c.LLVMDumpModule(self.llvm_context.module);
    }

    pub fn emit(self: *LLVMCodegen, path: [:0]const u8) !void {
        try self.llvm_context.emit_to_file(path);
    }

    pub fn generate(self: *LLVMCodegen) !void {
        for (self.program.statements.items) |*statement| {
            _ = try self.generate_node(statement, .{});
        }

        var error_msg: [*c]u8 = undefined;
        const verify_result = c.LLVMVerifyModule(self.llvm_context.module, c.LLVMAbortProcessAction, &error_msg);
        if (verify_result != 0) {
            std.debug.print("Module verification failed: {s}\n", .{error_msg});
            c.LLVMDisposeMessage(error_msg);
            return error.InvalidType;
        }
    }

    fn generate_node(self: *LLVMCodegen, node: *Node, options: CodegenOptions) CodegenError!CodegenResult {
        return switch (node.*) {
            .BlockStatement => |*block_statement| try self.generate_block_statement(block_statement),
            .VariableStatement => |*variable_statement| try self.generate_variable_statement(variable_statement),
            .FunctionStatement => |*function_statement| try self.generate_function_statement(function_statement),
            .StructStatement => |*struct_statement| try self.generate_struct_statement(struct_statement),
            .ReturnStatement => |*return_statement| try self.generate_return_statement(return_statement),
            .ExternStatement => |*extern_statement| try self.generate_extern_statement(extern_statement),
            .ExpressionStatement => |*expression_statement| try self.generate_expression(expression_statement.expression, options),
            else => return CodegenResult{
                .value = undefined,
                .context = .{ .void = {} },
            },
        };
    }

    fn generate_expression(self: *LLVMCodegen, node: *Node, options: CodegenOptions) CodegenError!CodegenResult {
        return switch (node.*) {
            .NumberLiteral => |*number_literal| CodegenResult{
                .value = c.LLVMConstInt(try self.to_llvm_type(options.type orelse number_literal.type), std.fmt.parseInt(c_ulonglong, number_literal.value, 10) catch return error.InvalidType, @intCast(0)),
                .context = .{ .void = {} },
            },
            .FloatLiteral => |*float_literal| CodegenResult{
                .value = c.LLVMConstReal(try self.to_llvm_type(options.type orelse float_literal.type), std.fmt.parseFloat(f64, float_literal.value) catch return error.InvalidType),
                .context = .{ .void = {} },
            },
            .StringLiteral => |*string_literal| blk: {
                const name_z = try self.allocator.dupeZ(u8, string_literal.value);
                defer self.allocator.free(name_z);

                break :blk CodegenResult{
                    .value = c.LLVMBuildGlobalStringPtr(self.llvm_context.builder, name_z.ptr, ""),
                    .context = .{ .void = {} },
                };
            },
            .BooleanLiteral => |*boolean_literal| CodegenResult{
                .value = c.LLVMConstInt(c.LLVMInt1TypeInContext(self.llvm_context.llvm), if (boolean_literal.value) 1 else 0, 0),
                .context = .{ .void = {} },
            },
            .Identifier => |*identifier| try self.generate_identifier(identifier, options),
            .BinaryExpression => |*binary_expression| try self.generate_binary_expression(binary_expression),
            .UnaryExpression => |*unary_expression| try self.generate_unary_expression(unary_expression, options),
            .CallExpression => |*call_expression| try self.generate_call_expression(call_expression),
            .StructLiteral => |*struct_literal| try self.generate_struct_literal(struct_literal, options),
            .MemberExpression => |*member_expression| try self.generate_member_expression(member_expression, options),
            .ArrayLiteral => |*array_literal| try self.generate_array_literal(array_literal, options),
            .IndexExpression => |*index_expression| try self.generate_index_expression(index_expression, options),
            .IfExpression => |*if_expression| try self.generate_if_expression(if_expression),
            else => return CodegenResult{
                .value = undefined,
                .context = .{ .void = {} },
            },
        };
    }

    fn generate_if_expression(self: *LLVMCodegen, expression: *node_zig.IfExpression) CodegenError!CodegenResult {
        const condition_result = try self.generate_expression(expression.condition, .{});
        const condition_value = condition_result.value;

        const llvm_function = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(self.llvm_context.builder));

        const then_block = c.LLVMAppendBasicBlock(llvm_function, "then");
        const else_block = c.LLVMAppendBasicBlock(llvm_function, "else");
        const merge_block = c.LLVMAppendBasicBlock(llvm_function, "merge");

        _ = c.LLVMBuildCondBr(self.llvm_context.builder, condition_value, then_block, else_block);

        c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, then_block);
        _ = try self.generate_node(expression.consequence, .{});

        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.llvm_context.builder)) == null) {
            _ = c.LLVMBuildBr(self.llvm_context.builder, merge_block);
        }

        c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, else_block);
        if (expression.alternative) |alternative| {
            _ = try self.generate_node(alternative, .{});
        }

        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.llvm_context.builder)) == null) {
            _ = c.LLVMBuildBr(self.llvm_context.builder, merge_block);
        }

        c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, merge_block);

        return CodegenResult{
            .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
            .context = .{ .void = {} },
        };
    }

    fn generate_block_statement(self: *LLVMCodegen, statement: *BlockStatement) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        env.set_current_scope(statement.scope);

        var last: c.LLVMValueRef = undefined;
        for (statement.statements.items) |*node| {
            const result = try self.generate_node(node, .{});
            last = result.value;
        }

        env.set_current_scope(current_scope);

        return CodegenResult{
            .value = last,
            .context = .{ .void = {} },
        };
    }

    fn generate_builtin_call(self: *LLVMCodegen, name: []const u8, expression: *CallExpression) CodegenError!CodegenResult {
        if (std.mem.eql(u8, name, "sizeof")) {
            const llvm_type = try self.to_llvm_type(expression.arguments.items[0].type);

            const type_size = c.LLVMSizeOfTypeInBits(self.llvm_context.data_layout.?, llvm_type);
            return CodegenResult{
                .value = c.LLVMConstInt(c.LLVMInt64TypeInContext(self.llvm_context.llvm), type_size, @intFromBool(false)),
                .context = .{ .void = {} },
            };
        }

        return CodegenResult{
            .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
            .context = .{ .void = {} },
        };
    }

    fn build_variadic_array(self: *LLVMCodegen, arguments: []const node_zig.Argument, element: Type) CodegenError!c.LLVMValueRef {
        const llvm_array_type = c.LLVMArrayType(try self.to_llvm_type(self.resolve_type(element)), @intCast(arguments.len));

        const array_ptr = try self.alloc(llvm_array_type);

        for (arguments, 0..) |argument, i| {
            const argument_result = try self.generate_expression(argument.value, .{});
            const argument_value = argument_result.value;

            var indices = [_]c.LLVMValueRef{
                c.LLVMConstInt(c.LLVMInt32TypeInContext(self.llvm_context.llvm), 0, 0),
                c.LLVMConstInt(c.LLVMInt32TypeInContext(self.llvm_context.llvm), @intCast(i), 0),
            };

            self.store(c.LLVMBuildGEP2(self.llvm_context.builder, llvm_array_type, array_ptr, &indices, 2, ""), argument_value);
        }

        return array_ptr;
    }

    fn build_call_arguments(
        self: *LLVMCodegen,
        arguments: []const node_zig.Argument,
        parameters: []const type_zig.FunctionParameterType,
        is_variadic: bool,
        is_llvm_variadic: bool,
    ) CodegenError!std.ArrayListUnmanaged(c.LLVMValueRef) {
        const required_params = if (is_variadic) parameters.len - 1 else parameters.len;

        var llvm_arguments = std.ArrayListUnmanaged(c.LLVMValueRef){};
        for (arguments, 0..) |argument, i| {
            if (i >= required_params and is_variadic and !is_llvm_variadic) break;

            const argument_result = try self.generate_expression(argument.value, .{});
            const argument_value = argument_result.value;

            try llvm_arguments.append(self.allocator, argument_value);
        }

        if (is_variadic and !is_llvm_variadic) {
            const variadic_type = self.resolve_type(parameters[parameters.len - 1].type.*);
            try llvm_arguments.append(self.allocator, try self.build_variadic_array(arguments[required_params..], variadic_type.Array.child.*));
        }

        return llvm_arguments;
    }

    fn build_llvm_function_type(
        self: *LLVMCodegen,
        parameters: []const type_zig.FunctionParameterType,
        return_type: Type,
        is_variadic: bool,
        is_llvm_variadic: bool,
    ) CodegenError!c.LLVMTypeRef {
        const required_params = if (is_variadic) parameters.len - 1 else parameters.len;

        var llvm_parameters = std.ArrayListUnmanaged(c.LLVMTypeRef){};
        defer llvm_parameters.deinit(self.allocator);
        for (parameters[0..required_params]) |parameter| {
            try llvm_parameters.append(self.allocator, try self.to_llvm_type(self.resolve_type(parameter.type.*)));
        }

        if (is_variadic and !is_llvm_variadic) {
            try llvm_parameters.append(self.allocator, try self.to_llvm_type(self.resolve_type(parameters[parameters.len - 1].type.*)));
        }

        return c.LLVMFunctionType(
            try self.to_llvm_type(self.resolve_type(return_type)),
            llvm_parameters.items.ptr,
            @intCast(llvm_parameters.items.len),
            @intFromBool(is_llvm_variadic),
        );
    }

    fn generate_call_expression(self: *LLVMCodegen, expression: *CallExpression) CodegenError!CodegenResult {
        if (expression.callee.* == .Identifier) {
            const name = expression.callee.Identifier.value;
            if (expression.generics.items.len > 0 and self.mono_cache.is_generic_function(name)) {
                return self.generate_monomorphized_call(expression, name);
            }

            for (builtin_zig.builtin_functions) |builtin_function| {
                if (std.mem.eql(u8, name, builtin_function)) {
                    return try self.generate_builtin_call(name, expression);
                }
            }
        }

        const llvm_function = try self.generate_expression(expression.callee, .{});

        const function_symbol = switch (llvm_function.context) {
            .function => |symbol| symbol,
            else => return error.InvalidType,
        };

        const is_llvm_variadic = c.LLVMIsFunctionVarArg(c.LLVMGlobalGetValueType(llvm_function.value)) != 0;

        var llvm_arguments = try self.build_call_arguments(expression.arguments.items, function_symbol.type.parameters, function_symbol.type.is_variadic, is_llvm_variadic);
        defer llvm_arguments.deinit(self.allocator);

        const llvm_function_type = try self.build_llvm_function_type(function_symbol.type.parameters, function_symbol.type.return_type.*, function_symbol.type.is_variadic, is_llvm_variadic);

        return CodegenResult{
            .value = c.LLVMBuildCall2(self.llvm_context.builder, llvm_function_type, llvm_function.value, llvm_arguments.items.ptr, @intCast(llvm_arguments.items.len), ""),
            .context = .{ .void = {} },
        };
    }

    fn generate_monomorphized_call(self: *LLVMCodegen, expression: *CallExpression, name: []const u8) CodegenError!CodegenResult {
        var generic_arguments = std.ArrayListUnmanaged(Type){};
        defer generic_arguments.deinit(self.allocator);
        for (expression.generics.items) |generic_argument| try generic_arguments.append(self.allocator, generic_argument.type.value);

        const cache_result = try self.mono_cache.get_or_create(name, generic_arguments.items);
        if (!cache_result.is_generated) {
            if (self.mono_cache.get_function(name)) |statement| {
                _ = try self.generate_monomorphized_function(statement, cache_result.name, generic_arguments.items);
                try self.mono_cache.mark_generated(cache_result.name);
            }
        }

        const name_z = try self.allocator.dupeZ(u8, cache_result.name);
        defer self.allocator.free(name_z);

        const function_statement = self.mono_cache.get_function(name) orelse return error.FunctionNotFound;

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();
        for (function_statement.proto.generics.items, 0..) |generic_parameter, i| {
            if (i < generic_arguments.items.len) try substitutions.put(generic_parameter.name.value, generic_arguments.items[i]);
        }

        const llvm_function = c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr) orelse return error.FunctionNotFound;

        const old_substitutions = self.substitutions;
        self.substitutions = substitutions;
        defer self.substitutions = old_substitutions;

        var parameters = std.ArrayListUnmanaged(type_zig.FunctionParameterType){};
        defer parameters.deinit(self.allocator);
        for (function_statement.proto.parameters.items) |parameter| {
            try parameters.append(self.allocator, type_zig.FunctionParameterType{ .name = parameter.name.value, .type = &parameter.type.value });
        }

        const is_variadic = for (function_statement.proto.parameters.items) |parameter| {
            if (parameter.is_variadic) break true;
        } else false;

        var llvm_arguments = try self.build_call_arguments(expression.arguments.items, parameters.items, is_variadic, false);
        defer llvm_arguments.deinit(self.allocator);

        const llvm_function_type = try self.build_llvm_function_type(parameters.items, function_statement.proto.return_type.value, is_variadic, false);

        return CodegenResult{
            .value = c.LLVMBuildCall2(self.llvm_context.builder, llvm_function_type, llvm_function, llvm_arguments.items.ptr, @intCast(llvm_arguments.items.len), ""),
            .context = .{ .void = {} },
        };
    }

    fn generate_struct(self: *LLVMCodegen, llvm_struct_type: c.LLVMTypeRef, fields: []const node_zig.StructLiteralField, options: CodegenOptions) CodegenError!CodegenResult {
        const struct_ptr = try self.alloc(llvm_struct_type);

        for (fields, 0..) |field, i| {
            const field_ptr = c.LLVMBuildStructGEP2(self.llvm_context.builder, llvm_struct_type, struct_ptr, @intCast(i), "");

            const field_result = try self.generate_expression(field.value, .{});
            const field_value = field_result.value;

            _ = c.LLVMBuildStore(self.llvm_context.builder, field_value, field_ptr);
        }

        return CodegenResult{
            .value = if (options.load) self.load(struct_ptr, llvm_struct_type) else struct_ptr,
            .context = .{ .void = {} },
        };
    }

    fn generate_struct_literal(self: *LLVMCodegen, literal: *node_zig.StructLiteral, options: CodegenOptions) CodegenError!CodegenResult {
        if (literal.generics.items.len > 0 and self.mono_cache.is_generic_struct(literal.name.value)) {
            return try self.generate_monomorphized_struct_literal(literal, literal.name.value, options);
        }

        const llvm_struct_type = try self.to_llvm_type(self.resolve_type(Type{ .Struct = literal.type.?.@"struct".* }));

        return try self.generate_struct(llvm_struct_type, literal.fields.items, options);
    }

    fn generate_monomorphized_struct_literal(self: *LLVMCodegen, literal: *node_zig.StructLiteral, name: []const u8, options: CodegenOptions) CodegenError!CodegenResult {
        var generic_arguments = std.ArrayListUnmanaged(Type){};
        defer generic_arguments.deinit(self.allocator);
        for (literal.generics.items) |generic_argument| try generic_arguments.append(self.allocator, generic_argument.type.value);

        const cache_result = try self.mono_cache.get_or_create(name, generic_arguments.items);
        if (!cache_result.is_generated) {
            if (self.mono_cache.get_struct(name)) |statement| {
                _ = try self.generate_monomorphized_struct_statement(statement, cache_result.name, generic_arguments.items);
                try self.mono_cache.mark_generated(cache_result.name);
            }
        }

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();
        for (literal.type.?.@"struct".generics, 0..) |generic_parameter, i| {
            if (i < generic_arguments.items.len) try substitutions.put(generic_parameter.name, generic_arguments.items[i]);
        }

        const old_substitutions = self.substitutions;
        self.substitutions = substitutions;
        defer self.substitutions = old_substitutions;

        const name_z = try self.allocator.dupeZ(u8, cache_result.name);
        defer self.allocator.free(name_z);

        const llvm_struct_type = c.LLVMGetTypeByName(self.llvm_context.module, name_z.ptr);

        return try self.generate_struct(llvm_struct_type, literal.fields.items, options);
    }

    fn generate_struct_definition(self: *LLVMCodegen, struct_type: type_zig.StructType, name: []const u8) CodegenError!CodegenResult {
        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        const llvm_struct_type = c.LLVMStructCreateNamed(self.llvm_context.llvm, name_z.ptr);

        var llvm_fields = std.ArrayListUnmanaged(c.LLVMTypeRef){};
        defer llvm_fields.deinit(self.allocator);
        for (struct_type.fields) |field| {
            try llvm_fields.append(self.allocator, try self.to_llvm_type(self.resolve_type(field.type.*)));
        }

        const owned_llvm_fields = try llvm_fields.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_llvm_fields);

        c.LLVMStructSetBody(llvm_struct_type, owned_llvm_fields.ptr, @intCast(owned_llvm_fields.len), 0);

        return CodegenResult{
            .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
            .context = .{ .@"struct" = llvm_struct_type },
        };
    }

    fn generate_struct_statement(self: *LLVMCodegen, statement: *node_zig.StructStatement) CodegenError!CodegenResult {
        if (statement.generics.items.len > 0) {
            try self.mono_cache.register_struct(statement.name.value, statement);

            return CodegenResult{
                .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
                .context = .{ .void = {} },
            };
        }

        return self.generate_struct_definition(statement.type.?, statement.name.value);
    }

    fn generate_monomorphized_struct_statement(self: *LLVMCodegen, statement: *node_zig.StructStatement, name: []const u8, generic_arguments: []const Type) CodegenError!CodegenResult {
        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();
        for (statement.generics.items, 0..) |generic_parameter, i| {
            if (i < generic_arguments.len) {
                try substitutions.put(generic_parameter.name.value, generic_arguments[i]);
            }
        }

        const old_substitutions = self.substitutions;
        self.substitutions = substitutions;
        defer self.substitutions = old_substitutions;

        return self.generate_struct_definition(statement.type.?, name);
    }

    fn generate_function(self: *LLVMCodegen, statement: *FunctionStatement, name: []const u8) CodegenError!CodegenResult {
        const llvm_function = try self.generate_function_proto_with_name(&statement.proto, name, false);

        const env = self.program.env;
        const current_scope = env.current_scope;

        env.set_current_scope(statement.body.scope);

        const body_block = c.LLVMAppendBasicBlock(llvm_function, "entry");
        c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, body_block);

        const scope_name = statement.body.scope.name;
        for (statement.proto.parameters.items, 0..) |*parameter, index| {
            const parameter_value = c.LLVMGetParam(llvm_function, @intCast(index));

            const parameter_name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ scope_name, parameter.name.value }) catch return error.OutOfMemory;

            const parameter_ptr = try self.alloc(try self.to_llvm_type(self.resolve_type(parameter.type.value)));
            self.store(parameter_ptr, parameter_value);

            try self.variables.put(parameter_name, parameter_ptr);
        }

        for (statement.body.statements.items) |*node| {
            _ = try self.generate_node(node, .{});
        }

        env.set_current_scope(current_scope);

        _ = c.LLVMVerifyFunction(llvm_function, c.LLVMAbortProcessAction);

        return CodegenResult{
            .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
            .context = .{ .void = {} },
        };
    }

    fn generate_monomorphized_function(
        self: *LLVMCodegen,
        statement: *FunctionStatement,
        name: []const u8,
        generic_arguments: []const Type,
    ) CodegenError!c.LLVMValueRef {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const current_block = c.LLVMGetInsertBlock(self.llvm_context.builder);

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();
        for (statement.proto.generics.items, 0..) |generic_parameter, index| {
            if (index < generic_arguments.len) {
                try substitutions.put(generic_parameter.name.value, generic_arguments[index]);
            }
        }

        const old_substitutions = self.substitutions;
        self.substitutions = substitutions;
        defer self.substitutions = old_substitutions;

        _ = try self.generate_function(statement, name);

        env.set_current_scope(current_scope);
        if (current_block != null) {
            c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, current_block);
        }

        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        return c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr);
    }

    fn generate_function_statement(self: *LLVMCodegen, statement: *FunctionStatement) CodegenError!CodegenResult {
        if (statement.proto.generics.items.len > 0) {
            try self.mono_cache.register_function(statement.proto.name.value, statement);

            return CodegenResult{
                .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
                .context = .{ .void = {} },
            };
        }

        var parameters = std.ArrayListUnmanaged(Type){};
        defer parameters.deinit(self.allocator);
        for (statement.proto.parameters.items) |param| {
            if (!param.is_variadic) try parameters.append(self.allocator, param.type.value);
        }

        const name = try mono_zig.mangle(self.allocator, statement.proto.name.value, parameters.items);
        defer self.allocator.free(name);

        return self.generate_function(statement, name);
    }

    fn generate_extern_statement(self: *LLVMCodegen, statement: *node_zig.ExternStatement) CodegenError!CodegenResult {
        switch (statement.*) {
            .Function => |*function| {
                return self.generate_function_proto(&function.proto);
            },
            .Variable => |*variable| {
                const name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ self.program.env.current_scope.name, variable.name.value }) catch return error.OutOfMemory;

                const name_z = try self.allocator.dupeZ(u8, name);
                defer self.allocator.free(name_z);

                const global = c.LLVMAddGlobal(self.llvm_context.module, try self.to_llvm_type(variable.type.value), name_z.ptr);
                c.LLVMSetLinkage(global, c.LLVMExternalLinkage);

                try self.variables.put(name, global);

                return CodegenResult{
                    .value = global,
                    .context = .{ .void = {} },
                };
            },
        }
    }

    fn generate_function_proto(self: *LLVMCodegen, statement: *FunctionProto) CodegenError!CodegenResult {
        const function = try self.generate_function_proto_with_name(statement, statement.name.value, true);

        const env = self.program.env;
        const current_scope = env.current_scope;

        const function_symbol = current_scope.lookup_first_function(statement.name.value) orelse return CodegenError.FunctionNotFound;

        return CodegenResult{
            .value = function,
            .context = .{ .function = function_symbol },
        };
    }

    fn generate_function_proto_with_name(self: *LLVMCodegen, statement: *FunctionProto, name: []const u8, is_extern: bool) CodegenError!c.LLVMValueRef {
        var is_variadic = false;

        var llvm_parameters = std.ArrayListUnmanaged(c.LLVMTypeRef){};
        defer llvm_parameters.deinit(self.allocator);
        for (statement.parameters.items) |parameter| {
            if (parameter.is_variadic) {
                is_variadic = true;

                if (is_extern) {
                    continue;
                }
            }

            try llvm_parameters.append(self.allocator, try self.to_llvm_type(self.resolve_type(parameter.type.value)));
        }

        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        const return_type = try self.to_llvm_type(self.resolve_type(statement.return_type.value));

        const owned_llvm_parameters = try llvm_parameters.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_llvm_parameters);

        const is_llvm_variadic = is_variadic and is_extern;

        const llvm_function_type = c.LLVMFunctionType(
            return_type,
            owned_llvm_parameters.ptr,
            @intCast(owned_llvm_parameters.len),
            @intFromBool(is_llvm_variadic),
        );

        return c.LLVMAddFunction(self.llvm_context.module, name_z.ptr, llvm_function_type);
    }

    fn generate_variable_statement(self: *LLVMCodegen, statement: *VariableStatement) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const variable_symbol = current_scope.lookup_variable(statement.name.value) orelse return CodegenError.VariableNotFound;

        var value: ?c.LLVMValueRef = null;

        if (statement.initializer) |initializer| {
            const result = try self.generate_expression(initializer, .{ .type = variable_symbol.type });
            value = result.value;
        }

        const name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ current_scope.name, variable_symbol.name }) catch return error.OutOfMemory;

        var ptr: c.LLVMValueRef = undefined;
        if (current_scope.is_global()) {
            const name_z = try self.allocator.dupeZ(u8, name);
            defer self.allocator.free(name_z);

            ptr = c.LLVMAddGlobal(self.llvm_context.module, try self.to_llvm_type(variable_symbol.type), name_z.ptr);
            c.LLVMSetGlobalConstant(ptr, @intFromBool(true));
            c.LLVMSetLinkage(ptr, c.LLVMExternalLinkage);

            if (value) |initializer| {
                c.LLVMSetInitializer(ptr, initializer);
            }
        } else {
            ptr = try self.alloc(try self.to_llvm_type(variable_symbol.type));

            if (value) |initializer| {
                self.store(ptr, initializer);
            }
        }

        try self.variables.put(name, ptr);

        return CodegenResult{
            .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
            .context = .{ .void = {} },
        };
    }

    fn generate_return_statement(self: *LLVMCodegen, statement: *ReturnStatement) CodegenError!CodegenResult {
        const result = try self.generate_expression(statement.value, .{});
        const value = result.value;

        return CodegenResult{
            .value = c.LLVMBuildRet(self.llvm_context.builder, value),
            .context = .{ .void = {} },
        };
    }

    fn generate_identifier(self: *LLVMCodegen, node: *Identifier, options: CodegenOptions) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        if (node.type == .Function) {
            const function_type = node.type.Function;

            const function_symbol = FunctionSymbol{
                .name = node.value,
                .visibility = .Public,
                .type = function_type,
            };

            for (builtin_zig.builtin_functions) |builtin_function| {
                if (std.mem.eql(u8, node.value, builtin_function)) {
                    return CodegenResult{
                        .value = c.LLVMGetUndef(try self.to_llvm_type(Type.void())),
                        .context = .{ .function = function_symbol },
                    };
                }
            }

            const parameter_count = if (function_type.is_variadic and function_type.parameters.len > 0) function_type.parameters.len - 1 else function_type.parameters.len;

            var parameter_types = std.ArrayListUnmanaged(Type){};
            defer parameter_types.deinit(self.allocator);
            for (function_type.parameters[0..parameter_count]) |param| {
                try parameter_types.append(self.allocator, param.type.*);
            }

            const mangled_name = try mono_zig.mangle(self.allocator, node.value, parameter_types.items);
            defer self.allocator.free(mangled_name);

            var llvm_function = blk: {
                const name_z = try self.allocator.dupeZ(u8, mangled_name);
                defer self.allocator.free(name_z);
                break :blk c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr);
            };

            if (llvm_function == null) {
                const name_z = try self.allocator.dupeZ(u8, node.value);
                defer self.allocator.free(name_z);
                llvm_function = c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr);
            }

            if (llvm_function == null) {
                return error.FunctionNotFound;
            }

            return CodegenResult{
                .value = llvm_function,
                .context = .{ .function = function_symbol },
            };
        }

        const variable_symbol = current_scope.lookup_variable(node.value) orelse return CodegenError.VariableNotFound;

        var value: ?c.LLVMValueRef = null;

        var scope: ?*scope_zig.Scope = current_scope;
        while (scope) |s| {
            const name = try std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ s.name, node.value });
            defer self.allocator.free(name);

            if (self.variables.get(name)) |v| {
                value = v;

                break;
            }

            scope = s.parent;
        }

        return CodegenResult{
            .value = if (options.load) self.load(value orelse return CodegenError.VariableNotFound, try self.to_llvm_type(variable_symbol.type)) else value orelse return CodegenError.VariableNotFound,
            .context = .{ .void = {} },
        };
    }

    fn generate_array_literal(self: *LLVMCodegen, literal: *ArrayLiteral, options: CodegenOptions) CodegenError!CodegenResult {
        const array_type = options.type orelse literal.type;
        const array_llvm_type = try self.to_llvm_type(array_type);

        const array_ptr = try self.alloc(array_llvm_type);

        for (literal.elements.items, 0..) |*element, i| {
            const result = try self.generate_expression(element, .{ .type = array_type.Array.child.* });

            var indices = [_]c.LLVMValueRef{
                c.LLVMConstInt(c.LLVMInt64TypeInContext(self.llvm_context.llvm), 0, 0),
                c.LLVMConstInt(c.LLVMInt64TypeInContext(self.llvm_context.llvm), i, 0),
            };

            const element_ptr = c.LLVMBuildGEP2(self.llvm_context.builder, array_llvm_type, array_ptr, &indices[0], 2, "");

            self.store(element_ptr, result.value);
        }

        return CodegenResult{
            .value = if (options.load) self.load(array_ptr, array_llvm_type) else array_ptr,
            .context = .{ .void = {} },
        };
    }

    fn generate_index_expression(self: *LLVMCodegen, expression: *IndexExpression, options: CodegenOptions) CodegenError!CodegenResult {
        const array_type = expression.array.get_type();

        const is_fixed = array_type.Array.size != null;

        const array_result = try self.generate_expression(expression.array, .{ .load = !is_fixed });
        const array_value = array_result.value;

        const index_result = try self.generate_expression(expression.index, .{});
        const index_value = index_result.value;

        const llvm_element_type = try self.to_llvm_type(expression.type);

        var ptr: c.LLVMValueRef = undefined;
        if (is_fixed) {
            const llvm_array_type = try self.to_llvm_type(array_type);
            var indices = [_]c.LLVMValueRef{
                c.LLVMConstInt(c.LLVMInt64TypeInContext(self.llvm_context.llvm), 0, 0),
                index_value,
            };

            ptr = c.LLVMBuildGEP2(self.llvm_context.builder, llvm_array_type, array_value, &indices[0], 2, "");
        } else {
            var indices = [_]c.LLVMValueRef{index_value};
            ptr = c.LLVMBuildGEP2(self.llvm_context.builder, llvm_element_type, array_value, &indices[0], 1, "");
        }

        return CodegenResult{
            .value = if (options.load) self.load(ptr, llvm_element_type) else ptr,
            .context = .{ .void = {} },
        };
    }

    fn generate_binary_expression(self: *LLVMCodegen, expression: *BinaryExpression) CodegenError!CodegenResult {
        var value: c.LLVMValueRef = undefined;

        switch (expression.operator.type) {
            .Assign => {
                const left_result = try self.generate_expression(expression.left, .{ .load = false });
                const left_value = left_result.value;

                const right_result = try self.generate_expression(expression.right, .{});
                const right_value = right_result.value;

                _ = c.LLVMBuildStore(self.llvm_context.builder, right_value, left_value);

                value = right_value;
            },
            .As => {
                const right_type = self.resolve_type(expression.right.get_type());
                const left_type = self.resolve_type(expression.left.get_type());

                const left_result = try self.generate_expression(expression.left, .{});
                const left_value = left_result.value;

                value = try self.cast_type(left_value, left_type, right_type);
            },
            else => {
                const left_result = try self.generate_expression(expression.left, .{});
                const right_result = try self.generate_expression(expression.right, .{});

                const left_value = left_result.value;
                const right_value = right_result.value;

                switch (expression.operator.type) {
                    .Plus => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFAdd(self.llvm_context.builder, left_value, right_value, "");
                        } else {
                            value = c.LLVMBuildAdd(self.llvm_context.builder, left_value, right_value, "");
                        }
                    },
                    .Minus => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFSub(self.llvm_context.builder, left_value, right_value, "");
                        } else {
                            value = c.LLVMBuildSub(self.llvm_context.builder, left_value, right_value, "");
                        }
                    },
                    .Asterisk => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFMul(self.llvm_context.builder, left_value, right_value, "");
                        } else {
                            value = c.LLVMBuildMul(self.llvm_context.builder, left_value, right_value, "");
                        }
                    },
                    .Divide => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFDiv(self.llvm_context.builder, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildUDiv(self.llvm_context.builder, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildSDiv(self.llvm_context.builder, left_value, right_value, "");
                            }
                        }
                    },
                    .Modulo => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFRem(self.llvm_context.builder, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildURem(self.llvm_context.builder, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildSRem(self.llvm_context.builder, left_value, right_value, "");
                            }
                        }
                    },
                    .And => value = c.LLVMBuildAnd(self.llvm_context.builder, left_value, right_value, ""),
                    .Or => value = c.LLVMBuildOr(self.llvm_context.builder, left_value, right_value, ""),
                    .Equals => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOEQ, left_value, right_value, "");
                        } else {
                            value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntEQ, left_value, right_value, "");
                        }
                    },
                    .NotEquals => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealONE, left_value, right_value, "");
                        } else {
                            value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntNE, left_value, right_value, "");
                        }
                    },
                    .LessThan => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOLT, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntULT, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSLT, left_value, right_value, "");
                            }
                        }
                    },
                    .GreaterThan => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOGT, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntUGT, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSGT, left_value, right_value, "");
                            }
                        }
                    },
                    .LessEqual => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOLE, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntULE, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSLE, left_value, right_value, "");
                            }
                        }
                    },
                    .GreaterEqual => {
                        if (expression.type == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOGE, left_value, right_value, "");
                        } else {
                            if (expression.type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntUGE, left_value, right_value, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSGE, left_value, right_value, "");
                            }
                        }
                    },
                    else => return error.InvalidType,
                }
            },
        }

        return CodegenResult{
            .value = value,
            .context = .{ .void = {} },
        };
    }

    fn generate_unary_expression(self: *LLVMCodegen, expression: *UnaryExpression, options: CodegenOptions) CodegenError!CodegenResult {
        const operand_result = try self.generate_expression(expression.operand, .{
            .load = expression.operator.type != .Ampersand,
        });
        const operand_value = operand_result.value;

        var value: c.LLVMValueRef = undefined;

        switch (expression.operator.type) {
            .Not => value = c.LLVMBuildNot(self.llvm_context.builder, operand_value, ""),
            .Minus => value = c.LLVMBuildNeg(self.llvm_context.builder, operand_value, ""),
            .Asterisk => {
                if (options.load) {
                    value = self.load(operand_value, try self.to_llvm_type(expression.type));
                } else {
                    value = operand_value;
                }
            },
            .Ampersand => {
                value = operand_value;
            },
            else => return error.InvalidType,
        }

        return CodegenResult{
            .value = value,
            .context = .{ .void = {} },
        };
    }

    fn generate_member_expression(self: *LLVMCodegen, expression: *node_zig.MemberExpression, options: CodegenOptions) CodegenError!CodegenResult {
        const object_result = try self.generate_expression(expression.object, .{ .load = false });
        const object_value = object_result.value;

        const object_type = expression.object.get_type();

        var found = false;

        var i: u32 = 0;
        switch (object_type) {
            .StructLiteral => |struct_literal| {
                for (struct_literal.@"struct".fields, 0..) |field, j| {
                    if (std.mem.eql(u8, field.name, expression.property.value)) {
                        i = @intCast(j);
                        found = true;
                        break;
                    }
                }
            },

            else => return error.InvalidType,
        }

        if (!found) return error.VariableNotFound;

        const ptr = c.LLVMBuildStructGEP2(self.llvm_context.builder, try self.to_llvm_type(object_type), object_value, i, "");

        return CodegenResult{
            .value = if (options.load) self.load(ptr, try self.to_llvm_type(expression.type)) else ptr,
            .context = .{ .void = {} },
        };
    }

    fn alloc(self: *LLVMCodegen, @"type": c.LLVMTypeRef) !c.LLVMValueRef {
        return c.LLVMBuildAlloca(self.llvm_context.builder, @"type", "");
    }

    fn store(self: *LLVMCodegen, ptr: c.LLVMValueRef, value: c.LLVMValueRef) void {
        _ = c.LLVMBuildStore(self.llvm_context.builder, value, ptr);
    }

    fn load(self: *LLVMCodegen, ptr: c.LLVMValueRef, @"type": c.LLVMTypeRef) c.LLVMValueRef {
        return c.LLVMBuildLoad2(self.llvm_context.builder, @"type", ptr, "");
    }

    fn cast_type(self: *LLVMCodegen, value: c.LLVMValueRef, from: Type, to: Type) CodegenError!c.LLVMValueRef {
        const to_type = try self.to_llvm_type(to);

        return switch (from) {
            .Integer => |from_int| switch (to) {
                .Integer => |to_int| blk: {
                    if (from_int.size == to_int.size) break :blk value;
                    if (from_int.size < to_int.size) {
                        if (from_int.is_unsigned) {
                            break :blk c.LLVMBuildZExt(self.llvm_context.builder, value, to_type, "");
                        } else {
                            break :blk c.LLVMBuildSExt(self.llvm_context.builder, value, to_type, "");
                        }
                    } else {
                        break :blk c.LLVMBuildTrunc(self.llvm_context.builder, value, to_type, "");
                    }
                },
                .Float => |to_float| blk: {
                    _ = to_float;
                    if (from_int.is_unsigned) {
                        break :blk c.LLVMBuildUIToFP(self.llvm_context.builder, value, to_type, "");
                    } else {
                        break :blk c.LLVMBuildSIToFP(self.llvm_context.builder, value, to_type, "");
                    }
                },
                else => value,
            },
            .Float => |from_float| switch (to) {
                .Integer => |to_int| blk: {
                    if (to_int.is_unsigned) {
                        break :blk c.LLVMBuildFPToUI(self.llvm_context.builder, value, to_type, "");
                    } else {
                        break :blk c.LLVMBuildFPToSI(self.llvm_context.builder, value, to_type, "");
                    }
                },
                .Float => |to_float| blk: {
                    if (from_float.size == to_float.size) break :blk value;
                    if (from_float.size < to_float.size) {
                        break :blk c.LLVMBuildFPExt(self.llvm_context.builder, value, to_type, "");
                    } else {
                        break :blk c.LLVMBuildFPTrunc(self.llvm_context.builder, value, to_type, "");
                    }
                },
                else => value,
            },
            .Pointer, .Array => switch (to) {
                .Pointer, .Array => c.LLVMBuildBitCast(self.llvm_context.builder, value, to_type, ""),
                else => value,
            },
            else => value,
        };
    }

    fn substitute_type(self: *LLVMCodegen, @"type": Type, substitutions: std.StringHashMap(Type)) Type {
        return switch (@"type") {
            .Named => |named| substitutions.get(named.name) orelse @"type",
            .Pointer => |pointer| blk: {
                const child = self.substitute_type(pointer.child.*, substitutions);
                const child_ptr = memory_zig.create(self.allocator, Type, child) catch break :blk @"type";
                break :blk Type{ .Pointer = .{ .child = child_ptr } };
            },
            .Array => |array| blk: {
                const child = self.substitute_type(array.child.*, substitutions);
                const child_ptr = memory_zig.create(self.allocator, Type, child) catch break :blk @"type";
                break :blk Type{ .Array = .{ .child = child_ptr, .size = array.size } };
            },
            else => @"type",
        };
    }

    fn resolve_type(self: *LLVMCodegen, @"type": Type) Type {
        if (self.substitutions) |substitutions| {
            return self.substitute_type(@"type", substitutions);
        }

        return @"type";
    }
};
