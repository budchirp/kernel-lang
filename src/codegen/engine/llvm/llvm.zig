const std = @import("std");

const c = @import("c.zig").c;

const context_zig = @import("context.zig");
const node_zig = @import("../../../ast/node.zig");
const program_zig = @import("../../../ast/program.zig");
const type_zig = @import("../../../types/type.zig");
const compiler_zig = @import("../../../compiler/context.zig");
const symbol_zig = @import("../../../symbol/symbol.zig");
const mono_zig = @import("../../monomorphization.zig");
const memory_zig = @import("../../../utils/memory.zig");

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

const LLVMConverter = struct {
    allocator: std.mem.Allocator,

    context: LLVMCodegenContext,
    program: *Program,

    substitutions: ?std.StringHashMap(Type) = null,

    pub fn from_type(self: *LLVMConverter, @"type": Type) !c.LLVMTypeRef {
        const resolved_type = if (self.substitutions) |substitutions|
            self.apply_substitutions(@"type", substitutions)
        else
            @"type";

        switch (resolved_type) {
            .Integer => |integer| {
                return switch (integer.size) {
                    8 => c.LLVMInt8TypeInContext(self.context.llvm),
                    16 => c.LLVMInt16TypeInContext(self.context.llvm),
                    32 => c.LLVMInt32TypeInContext(self.context.llvm),
                    64 => c.LLVMInt64TypeInContext(self.context.llvm),
                    else => error.InvalidType,
                };
            },
            .Float => |float| {
                return switch (float.size) {
                    32 => c.LLVMFloatTypeInContext(self.context.llvm),
                    64 => c.LLVMDoubleTypeInContext(self.context.llvm),
                    else => error.InvalidType,
                };
            },
            .Boolean => return c.LLVMInt1TypeInContext(self.context.llvm),
            .String => return c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context.llvm), 0),
            .Array => |array| {
                if (array.size) |size| {
                    return c.LLVMArrayType(try self.from_type(array.child.*), @intCast(size));
                } else {
                    return c.LLVMPointerType(try self.from_type(array.child.*), 0);
                }
            },
            .Pointer => |pointer| return c.LLVMPointerType(try self.from_type(pointer.child.*), 0),
            .Function => |function| {
                var parameters = std.ArrayListUnmanaged(c.LLVMTypeRef){};
                defer parameters.deinit(self.allocator);

                const param_count = if (function.is_variadic and function.parameters.len > 0) function.parameters.len - 1 else function.parameters.len;
                for (function.parameters[0..param_count]) |parameter| {
                    try parameters.append(self.allocator, try self.from_type(parameter.type.*));
                }

                const owned_parameters = try parameters.toOwnedSlice(self.allocator);
                defer self.allocator.free(owned_parameters);

                return c.LLVMFunctionType(try self.from_type(function.return_type.*), owned_parameters.ptr, @intCast(owned_parameters.len), @intFromBool(function.is_variadic));
            },
            .Struct => |@"struct"| {
                var fields = std.ArrayListUnmanaged(c.LLVMTypeRef){};
                defer fields.deinit(self.allocator);

                for (@"struct".fields) |field| {
                    try fields.append(self.allocator, try self.from_type(field.type.*));
                }

                const owned_fields = try fields.toOwnedSlice(self.allocator);
                defer self.allocator.free(owned_fields);

                return c.LLVMStructTypeInContext(self.context.llvm, owned_fields.ptr, @intCast(owned_fields.len), 0);
            },
            .Named => |named| {
                if (self.substitutions) |substitutions| {
                    if (substitutions.get(named.name)) |substituted| {
                        return self.from_type(substituted);
                    }
                }

                if (self.program.env.current_scope.lookup_type(named.name)) |symbol| {
                    return self.from_type(symbol.type);
                }

                return c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context.llvm), 0);
            },
            .Void => return c.LLVMVoidTypeInContext(self.context.llvm),
            else => return error.InvalidType,
        }
    }

    fn apply_substitutions(self: *LLVMConverter, @"type": Type, substitutions: std.StringHashMap(Type)) Type {
        _ = self;
        return switch (@"type") {
            .Named => |named| substitutions.get(named.name) orelse @"type",
            else => @"type",
        };
    }
};

pub const CodegenError = error{
    InvalidType,
    OutOfMemory,
    VariableNotFound,
    FunctionNotFound,
};

pub const CodegenResultContext = union(enum) {
    void: void,
    function: FunctionSymbol,
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

    converter: LLVMConverter,

    program: *Program,

    variables: std.StringHashMap(c.LLVMValueRef),

    mono_cache: MonomorphizationCache,

    pub fn init(allocator: std.mem.Allocator, name: [:0]const u8, context: CompilerContext, program: *Program) !LLVMCodegen {
        const llvm_context = try LLVMCodegenContext.init(name);

        const converter = LLVMConverter{
            .allocator = allocator,
            .context = llvm_context,
            .program = program,
        };

        return LLVMCodegen{
            .allocator = allocator,
            .context = context,
            .llvm_context = llvm_context,
            .converter = converter,
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
            return error.InvalidType; // Use a generic error
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
                .value = c.LLVMConstInt(try self.converter.from_type(options.type orelse number_literal.type), std.fmt.parseInt(c_ulonglong, number_literal.value, 10) catch return error.InvalidType, @intCast(0)),
                .context = .{ .void = {} },
            },
            .FloatLiteral => |*float_literal| CodegenResult{
                .value = c.LLVMConstReal(try self.converter.from_type(options.type orelse float_literal.type), std.fmt.parseFloat(f64, float_literal.value) catch return error.InvalidType),
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
            .StructLiteral => |*struct_literal| try self.generate_struct_literal(struct_literal),
            .MemberExpression => |*member_expression| try self.generate_member_expression(member_expression, options),
            .ArrayLiteral => |*array_literal| try self.generate_array_literal(array_literal, options),
            .IndexExpression => |*index_expression| try self.generate_index_expression(index_expression, options),
            else => return CodegenResult{
                .value = undefined,
                .context = .{ .void = {} },
            },
        };
    }

    fn generate_block_statement(self: *LLVMCodegen, statement: *BlockStatement) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        env.set_current_scope(&statement.scope);

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

    fn generate_function_statement(self: *LLVMCodegen, statement: *FunctionStatement) CodegenError!CodegenResult {
        if (statement.proto.generics.items.len > 0) {
            try self.mono_cache.register_generic(statement.proto.name.value, statement);

            return CodegenResult{
                .value = c.LLVMGetUndef(self.void()),
                .context = .{ .void = {} },
            };
        }

        var parameter_types = std.ArrayListUnmanaged(Type){};
        defer parameter_types.deinit(self.allocator);
        for (statement.proto.parameters.items) |param| {
            if (!param.is_variadic) try parameter_types.append(self.allocator, param.type.value);
        }

        const mangled_name = try mono_zig.mangle(self.allocator, statement.proto.name.value, parameter_types.items);
        defer self.allocator.free(mangled_name);

        return self.generate_function(statement, mangled_name);
    }

    fn generate_function(self: *LLVMCodegen, statement: *FunctionStatement, name: []const u8) CodegenError!CodegenResult {
        const function = try self.generate_function_proto_with_name(&statement.proto, name);

        const env = self.program.env;
        const current_scope = env.current_scope;

        env.set_current_scope(&statement.body.scope);

        const body = c.LLVMAppendBasicBlock(function, "entry");
        c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, body);

        const scope_name = statement.body.scope.name;
        for (statement.proto.parameters.items, 0..) |*parameter, index| {
            const parameter_value = c.LLVMGetParam(function, @intCast(index));
            const parameter_name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ scope_name, parameter.name.value }) catch return error.OutOfMemory;

            const parameter_type = if (self.converter.substitutions) |substitutions|
                self.substitute_type_value(parameter.type.value, substitutions)
            else
                parameter.type.value;

            const ptr = try self.alloc(try self.converter.from_type(parameter_type));
            self.store(ptr, parameter_value);

            try self.variables.put(parameter_name, ptr);
        }

        for (statement.body.statements.items) |*node| {
            _ = try self.generate_node(node, .{});
        }

        env.set_current_scope(current_scope);

        _ = c.LLVMVerifyFunction(function, c.LLVMAbortProcessAction);

        return CodegenResult{
            .value = c.LLVMGetUndef(self.void()),
            .context = .{ .void = {} },
        };
    }

    fn generate_monomorphized_function(
        self: *LLVMCodegen,
        statement: *FunctionStatement,
        name: []const u8,
        types: []const Type,
    ) CodegenError!c.LLVMValueRef {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const current_block = c.LLVMGetInsertBlock(self.llvm_context.builder);

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        for (statement.proto.generics.items, 0..) |generic, index| {
            if (index < types.len) {
                try substitutions.put(generic.name.value, types[index]);
            }
        }

        const old_substitutions = self.converter.substitutions;
        self.converter.substitutions = substitutions;

        defer {
            self.converter.substitutions = old_substitutions;
            substitutions.deinit();
        }

        _ = try self.generate_function(statement, name);

        env.set_current_scope(current_scope);
        if (current_block != null) {
            c.LLVMPositionBuilderAtEnd(self.llvm_context.builder, current_block);
        }

        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        return c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr);
    }

    fn substitute_type_value(self: *LLVMCodegen, @"type": Type, substitutions: std.StringHashMap(Type)) Type {
        return switch (@"type") {
            .Named => |named| substitutions.get(named.name) orelse @"type",
            .Pointer => |pointer| blk: {
                const child = self.substitute_type_value(pointer.child.*, substitutions);
                const child_ptr = memory_zig.create(self.allocator, Type, child) catch break :blk @"type";
                break :blk Type{ .Pointer = .{ .child = child_ptr } };
            },
            .Array => |array| blk: {
                const child = self.substitute_type_value(array.child.*, substitutions);
                const child_ptr = memory_zig.create(self.allocator, Type, child) catch break :blk @"type";
                break :blk Type{ .Array = .{ .child = child_ptr, .size = array.size } };
            },
            else => @"type",
        };
    }

    fn generate_struct_statement(self: *LLVMCodegen, struct_statement: *node_zig.StructStatement) CodegenError!CodegenResult {
        _ = self;
        _ = struct_statement;
        return CodegenResult{
            .value = undefined,
            .context = .{ .void = {} },
        };
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

                const global = c.LLVMAddGlobal(self.llvm_context.module, try self.converter.from_type(variable.type.value), name_z.ptr);
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
        const function = try self.generate_function_proto_with_name(statement, statement.name.value);

        const env = self.program.env;
        const current_scope = env.current_scope;
        const function_symbol = current_scope.lookup_function(statement.name.value) orelse return CodegenError.FunctionNotFound;

        return CodegenResult{
            .value = function,
            .context = .{ .function = function_symbol },
        };
    }

    fn generate_function_proto_with_name(self: *LLVMCodegen, statement: *FunctionProto, name: []const u8) CodegenError!c.LLVMValueRef {
        var parameters = std.ArrayListUnmanaged(c.LLVMTypeRef){};
        defer parameters.deinit(self.allocator);

        var is_variadic = false;
        for (statement.parameters.items) |parameter| {
            if (parameter.is_variadic) {
                is_variadic = true;
                continue;
            }

            const parameter_type = if (self.converter.substitutions) |substitutions|
                self.substitute_type_value(parameter.type.value, substitutions)
            else
                parameter.type.value;

            try parameters.append(self.allocator, try self.converter.from_type(parameter_type));
        }

        const return_type = if (self.converter.substitutions) |substitutions|
            self.substitute_type_value(statement.return_type.value, substitutions)
        else
            statement.return_type.value;

        const owned_parameters = try parameters.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_parameters);

        const function_type = c.LLVMFunctionType(
            try self.converter.from_type(return_type),
            owned_parameters.ptr,
            @intCast(owned_parameters.len),
            @intFromBool(is_variadic),
        );

        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        return c.LLVMAddFunction(self.llvm_context.module, name_z.ptr, function_type);
    }

    fn generate_variable_statement(self: *LLVMCodegen, statement: *VariableStatement) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const variable_symbol = current_scope.lookup_variable(statement.name.value) orelse return CodegenError.VariableNotFound;

        var value: ?c.LLVMValueRef = null;

        if (statement.initializer) |initializer| {
            const result = try self.generate_expression(initializer, .{ .type = variable_symbol.type });
            value = try self.cast_type(result.value, initializer.get_type(), variable_symbol.type);
        }

        const name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ current_scope.name, variable_symbol.name }) catch return error.OutOfMemory;

        var variable: c.LLVMValueRef = undefined;
        if (current_scope.is_global()) {
            const name_z = try self.allocator.dupeZ(u8, name);
            defer self.allocator.free(name_z);

            variable = c.LLVMAddGlobal(self.llvm_context.module, try self.converter.from_type(variable_symbol.type), name_z.ptr);
            c.LLVMSetGlobalConstant(variable, @intCast(1));
            c.LLVMSetLinkage(variable, c.LLVMExternalLinkage);

            if (value) |initializer| {
                c.LLVMSetInitializer(variable, initializer);
            }
        } else {
            variable = try self.alloc(try self.converter.from_type(variable_symbol.type));

            if (value) |initializer| {
                self.store(variable, initializer);
            }
        }

        try self.variables.put(name, variable);

        return CodegenResult{
            .value = c.LLVMGetUndef(self.void()),
            .context = .{ .void = {} },
        };
    }

    fn generate_return_statement(self: *LLVMCodegen, statement: *ReturnStatement) CodegenError!CodegenResult {
        const result = try self.generate_expression(statement.value, .{});
        var value = result.value;

        const env = self.program.env;
        const current_scope = env.current_scope;

        if (current_scope.type == .Function) {
            if (current_scope.function) |function| {
                const value_type = self.resolve_expression_type(statement.value.get_type());
                const return_type = self.resolve_expression_type(function.type.return_type.*);

                value = try self.cast_type(value, value_type, return_type);
            }
        }

        return CodegenResult{
            .value = c.LLVMBuildRet(self.llvm_context.builder, value),
            .context = .{ .void = {} },
        };
    }

    fn generate_identifier(self: *LLVMCodegen, node: *Identifier, options: CodegenOptions) CodegenError!CodegenResult {
        const env = self.program.env;
        const current_scope = env.current_scope;

        const name = std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ current_scope.name, node.value }) catch return error.OutOfMemory;
        defer self.allocator.free(name);

        if (node.type == .Function) {
            const function_type = node.type.Function;

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
                .context = .{ .function = FunctionSymbol{
                    .name = node.value,
                    .visibility = .Public,
                    .type = function_type,
                    .is_operator = false,
                } },
            };
        }

        const variable_symbol = current_scope.lookup_variable(node.value) orelse return CodegenError.VariableNotFound;

        const value = self.variables.get(name) orelse return CodegenError.VariableNotFound;

        return CodegenResult{
            .value = if (options.load) self.load(value, try self.converter.from_type(variable_symbol.type)) else value,
            .context = .{ .void = {} },
        };
    }

    fn generate_array_literal(self: *LLVMCodegen, literal: *ArrayLiteral, options: CodegenOptions) CodegenError!CodegenResult {
        const array_type = options.type orelse literal.type;
        const array_llvm_type = try self.converter.from_type(array_type);

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
        const array = array_result.value;

        const index_result = try self.generate_expression(expression.index, .{});
        const index = index_result.value;

        const llvm_element_type = try self.converter.from_type(expression.type);

        var ptr: c.LLVMValueRef = undefined;
        if (is_fixed) {
            const llvm_array_type = try self.converter.from_type(array_type);
            var indices = [_]c.LLVMValueRef{
                c.LLVMConstInt(c.LLVMInt64TypeInContext(self.llvm_context.llvm), 0, 0),
                index,
            };

            ptr = c.LLVMBuildGEP2(self.llvm_context.builder, llvm_array_type, array, &indices[0], 2, "");
        } else {
            var indices = [_]c.LLVMValueRef{index};
            ptr = c.LLVMBuildGEP2(self.llvm_context.builder, llvm_element_type, array, &indices[0], 1, "");
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
                const left_type = self.resolve_expression_type(expression.left.get_type());

                const left_result = try self.generate_expression(expression.left, .{ .load = false });
                const left = left_result.value;

                const right_result = try self.generate_expression(expression.right, .{});
                const right_type = self.resolve_expression_type(expression.right.get_type());
                const right = try self.cast_type(right_result.value, right_type, left_type);

                _ = c.LLVMBuildStore(self.llvm_context.builder, right, left);

                value = right;
            },
            .As => {
                const right_type = self.resolve_expression_type(expression.right.get_type());
                const left_type = self.resolve_expression_type(expression.left.get_type());

                const left_result = try self.generate_expression(expression.left, .{});
                const left = left_result.value;

                value = try self.cast_type(left, left_type, right_type);
            },
            else => {
                const left_result = try self.generate_expression(expression.left, .{});

                const right_result = try self.generate_expression(expression.right, .{});

                const left_type = self.resolve_expression_type(expression.left.get_type());
                const right_type = self.resolve_expression_type(expression.right.get_type());

                const common_type = self.resolve_common_type(left_type, right_type);

                const left = try self.cast_type(left_result.value, left_type, common_type);
                const right = try self.cast_type(right_result.value, right_type, common_type);

                switch (expression.operator.type) {
                    .Plus => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFAdd(self.llvm_context.builder, left, right, "");
                        } else {
                            value = c.LLVMBuildAdd(self.llvm_context.builder, left, right, "");
                        }
                    },
                    .Minus => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFSub(self.llvm_context.builder, left, right, "");
                        } else {
                            value = c.LLVMBuildSub(self.llvm_context.builder, left, right, "");
                        }
                    },
                    .Asterisk => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFMul(self.llvm_context.builder, left, right, "");
                        } else {
                            value = c.LLVMBuildMul(self.llvm_context.builder, left, right, "");
                        }
                    },
                    .Divide => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFDiv(self.llvm_context.builder, left, right, "");
                        } else {
                            // Assuming signed for now for simplicity, or check is_unsigned
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildUDiv(self.llvm_context.builder, left, right, "");
                            } else {
                                value = c.LLVMBuildSDiv(self.llvm_context.builder, left, right, "");
                            }
                        }
                    },
                    .Modulo => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFRem(self.llvm_context.builder, left, right, "");
                        } else {
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildURem(self.llvm_context.builder, left, right, "");
                            } else {
                                value = c.LLVMBuildSRem(self.llvm_context.builder, left, right, "");
                            }
                        }
                    },
                    .And => value = c.LLVMBuildAnd(self.llvm_context.builder, left, right, ""),
                    .Or => value = c.LLVMBuildOr(self.llvm_context.builder, left, right, ""),
                    .Equals => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOEQ, left, right, "");
                        } else {
                            value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntEQ, left, right, "");
                        }
                    },
                    .NotEquals => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealONE, left, right, "");
                        } else {
                            value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntNE, left, right, "");
                        }
                    },
                    .LessThan => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOLT, left, right, "");
                        } else {
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntULT, left, right, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSLT, left, right, "");
                            }
                        }
                    },
                    .GreaterThan => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOGT, left, right, "");
                        } else {
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntUGT, left, right, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSGT, left, right, "");
                            }
                        }
                    },
                    .LessEqual => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOLE, left, right, "");
                        } else {
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntULE, left, right, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSLE, left, right, "");
                            }
                        }
                    },
                    .GreaterEqual => {
                        if (std.meta.activeTag(common_type) == .Float) {
                            value = c.LLVMBuildFCmp(self.llvm_context.builder, c.LLVMRealOGE, left, right, "");
                        } else {
                            if (common_type.Integer.is_unsigned) {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntUGE, left, right, "");
                            } else {
                                value = c.LLVMBuildICmp(self.llvm_context.builder, c.LLVMIntSGE, left, right, "");
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
        const operand = operand_result.value;

        var value: c.LLVMValueRef = undefined;

        switch (expression.operator.type) {
            .Not => value = c.LLVMBuildNot(self.llvm_context.builder, operand, ""),
            .Minus => value = c.LLVMBuildNeg(self.llvm_context.builder, operand, ""),
            .Asterisk => {
                if (options.load) {
                    value = self.load(operand, try self.converter.from_type(expression.type));
                } else {
                    value = operand;
                }
            },
            .Ampersand => {
                value = operand;
            },
            else => return error.InvalidType,
        }

        return CodegenResult{
            .value = value,
            .context = .{ .void = {} },
        };
    }

    fn generate_call_expression(self: *LLVMCodegen, expression: *CallExpression) CodegenError!CodegenResult {
        const callee_name = switch (expression.callee.*) {
            .Identifier => |identifier| identifier.value,
            else => null,
        };

        if (callee_name) |name| {
            if (expression.generics.items.len > 0 and self.mono_cache.is_generic(name)) {
                return self.generate_monomorphized_call(expression, name);
            }
        }

        const callee = try self.generate_expression(expression.callee, .{});

        const function_symbol = switch (callee.context) {
            .function => |function| function,
            else => return error.InvalidType,
        };

        var arguments = std.ArrayListUnmanaged(c.LLVMValueRef){};
        defer arguments.deinit(self.allocator);

        for (expression.arguments.items, 0..) |argument, index| {
            const value_result = try self.generate_expression(argument.value, .{});
            var value = value_result.value;

            if (index < function_symbol.type.parameters.len) {
                const parameter_type = function_symbol.type.parameters[index].type.*;
                var argument_type = argument.value.get_type();

                const env = self.program.env;
                if (argument.value.* == .Identifier) {
                    if (env.current_scope.lookup_variable(argument.value.Identifier.value)) |symbol| {
                        argument_type = symbol.type;
                    }
                }

                value = try self.cast_type(value, argument_type, parameter_type);
            }

            try arguments.append(self.allocator, value);
        }

        const owned_arguments = try arguments.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_arguments);

        const function_type = try self.converter.from_type(Type{ .Function = function_symbol.type });

        return CodegenResult{
            .value = c.LLVMBuildCall2(
                self.llvm_context.builder,
                function_type,
                callee.value,
                owned_arguments.ptr,
                @intCast(owned_arguments.len),
                "",
            ),
            .context = .{ .void = {} },
        };
    }

    fn generate_monomorphized_call(self: *LLVMCodegen, expression: *CallExpression, name: []const u8) CodegenError!CodegenResult {
        var types = std.ArrayListUnmanaged(Type){};
        defer types.deinit(self.allocator);

        for (expression.generics.items) |generic_argument| {
            try types.append(self.allocator, generic_argument.type.value);
        }

        const specialization_result = try self.mono_cache.get_or_create(name, types.items);

        const mangled = specialization_result.name;

        if (!specialization_result.is_generated) {
            if (self.mono_cache.get_generic(name)) |generic_function| {
                _ = try self.generate_monomorphized_function(generic_function, mangled, types.items);

                try self.mono_cache.mark_generated(mangled);
            }
        }

        const name_z = try self.allocator.dupeZ(u8, mangled);
        defer self.allocator.free(name_z);

        const function = c.LLVMGetNamedFunction(self.llvm_context.module, name_z.ptr);
        if (function == null) {
            return error.FunctionNotFound;
        }

        const generic_function = self.mono_cache.get_generic(name) orelse return error.FunctionNotFound;

        var substitutions = std.StringHashMap(Type).init(self.allocator);
        defer substitutions.deinit();
        for (generic_function.proto.generics.items, 0..) |generic, index| {
            if (index < types.items.len) {
                try substitutions.put(generic.name.value, types.items[index]);
            }
        }

        var arguments = std.ArrayListUnmanaged(c.LLVMValueRef){};
        defer arguments.deinit(self.allocator);
        for (expression.arguments.items, 0..) |argument, index| {
            const value_result = try self.generate_expression(argument.value, .{});
            var value = value_result.value;

            if (index < generic_function.proto.parameters.items.len) {
                const parameter_type = self.substitute_type_value(generic_function.proto.parameters.items[index].type.value, substitutions);
                const argument_type = argument.value.get_type();

                value = try self.cast_type(value, argument_type, parameter_type);
            }

            try arguments.append(self.allocator, value);
        }

        var parameter_types = std.ArrayListUnmanaged(c.LLVMTypeRef){};
        defer parameter_types.deinit(self.allocator);
        for (generic_function.proto.parameters.items) |parameter| {
            const parameter_type = self.substitute_type_value(parameter.type.value, substitutions);

            try parameter_types.append(self.allocator, try self.converter.from_type(parameter_type));
        }

        const return_type = self.substitute_type_value(generic_function.proto.return_type.value, substitutions);

        const owned_parameter_types = try parameter_types.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_parameter_types);

        const function_type = c.LLVMFunctionType(
            try self.converter.from_type(return_type),
            owned_parameter_types.ptr,
            @intCast(owned_parameter_types.len),
            @intCast(0),
        );

        const owned_arguments = try arguments.toOwnedSlice(self.allocator);

        return CodegenResult{
            .value = c.LLVMBuildCall2(
                self.llvm_context.builder,
                function_type,
                function,
                owned_arguments.ptr,
                @intCast(owned_arguments.len),
                "",
            ),
            .context = .{ .void = {} },
        };
    }

    fn generate_struct_literal(self: *LLVMCodegen, literal: *node_zig.StructLiteral) CodegenError!CodegenResult {
        _ = try self.converter.from_type(literal.type);

        var values = std.ArrayListUnmanaged(c.LLVMValueRef){};
        defer values.deinit(self.allocator);

        switch (literal.type) {
            .Struct => |@"struct"| {
                for (@"struct".fields) |field_type| {
                    var found = false;

                    for (literal.fields.items) |field| {
                        if (std.mem.eql(u8, field.name.value, field_type.name)) {
                            const result = try self.generate_expression(field.value, .{});
                            try values.append(self.allocator, result.value);

                            found = true;

                            break;
                        }
                    }

                    if (!found) return error.InvalidType;
                }
            },
            else => return error.InvalidType,
        }

        const owned_values = try values.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_values);

        const value = c.LLVMConstStructInContext(self.llvm_context.llvm, owned_values.ptr, @intCast(owned_values.len), 0);

        return CodegenResult{
            .value = value,
            .context = .{ .void = {} },
        };
    }

    fn generate_member_expression(self: *LLVMCodegen, expression: *node_zig.MemberExpression, options: CodegenOptions) CodegenError!CodegenResult {
        const object_result = try self.generate_expression(expression.object, .{ .load = false });
        const object = object_result.value;

        const object_type = expression.object.get_type();

        var index: u32 = 0;
        var found = false;

        switch (object_type) {
            .Struct => |@"struct"| {
                for (@"struct".fields, 0..) |field, i| {
                    if (std.mem.eql(u8, field.name, expression.property.value)) {
                        index = @intCast(i);
                        found = true;
                        break;
                    }
                }
            },

            else => return error.InvalidType,
        }

        if (!found) return error.VariableNotFound;

        const ptr = c.LLVMBuildStructGEP2(self.llvm_context.builder, try self.converter.from_type(object_type), object, index, "");

        return CodegenResult{
            .value = if (options.load) self.load(ptr, try self.converter.from_type(expression.type)) else ptr,
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

    fn @"void"(self: *LLVMCodegen) c.LLVMTypeRef {
        return c.LLVMVoidTypeInContext(self.llvm_context.llvm);
    }

    fn cast_type(self: *LLVMCodegen, value: c.LLVMValueRef, from: Type, to: Type) CodegenError!c.LLVMValueRef {
        const to_type = try self.converter.from_type(to);

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

    fn resolve_expression_type(self: *LLVMCodegen, @"type": Type) Type {
        if (self.converter.substitutions) |substitutions| {
            return self.substitute_type_value(@"type", substitutions);
        }

        return @"type";
    }

    fn resolve_common_type(self: *LLVMCodegen, left: Type, right: Type) Type {
        _ = self;
        if (left == .Float or right == .Float) {
            if (left == .Float and right == .Float) {
                if (left.Float.size >= right.Float.size) return left else return right;
            }

            if (left == .Float) return left;

            return right;
        }

        if (left == .Integer and right == .Integer) {
            if (left.Integer.size >= right.Integer.size) return left else return right;
        }

        return left;
    }
};
