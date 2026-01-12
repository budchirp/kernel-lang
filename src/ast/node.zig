const std = @import("std");

const position_zig = @import("../lexer/position.zig");
const token_type_zig = @import("../lexer/token/token_type.zig");
const type_zig = @import("../types/type.zig");
const scope_zig = @import("../symbol/scope.zig");
const token_zig = @import("../lexer/token/token.zig");

const Position = position_zig.Position;
const Token = token_zig.Token;
const TokenType = token_type_zig.TokenType;
const Type = type_zig.Type;
const Scope = scope_zig.Scope;

pub const Node = union(enum) {
    ImportStatement: ImportStatement,
    ExternStatement: ExternStatement,
    StructStatement: StructStatement,
    FunctionStatement: FunctionStatement,
    VariableStatement: VariableStatement,
    IfExpression: IfExpression,
    ForStatement: ForStatement,
    WhileStatement: WhileStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,
    BinaryExpression: BinaryExpression,
    UnaryExpression: UnaryExpression,
    CallExpression: CallExpression,
    MemberExpression: MemberExpression,
    IndexExpression: IndexExpression,
    StructLiteral: StructLiteral,
    TypeExpression: TypeExpression,
    Identifier: Identifier,
    NumberLiteral: NumberLiteral,
    FloatLiteral: FloatLiteral,
    StringLiteral: StringLiteral,
    BooleanLiteral: BooleanLiteral,
    ArrayLiteral: ArrayLiteral,

    pub fn position(self: Node) Position {
        return switch (self) {
            .ImportStatement => |s| s.position,
            .StructStatement => |s| s.position,
            .ExternStatement => |e| e.position(),
            .FunctionStatement => |f| f.position,
            .VariableStatement => |v| v.position,
            .IfExpression => |i| i.position,
            .ForStatement => |f| f.position,
            .WhileStatement => |w| w.position,
            .ReturnStatement => |r| r.position,
            .ExpressionStatement => |e| e.position,
            .BlockStatement => |b| b.position,
            .BinaryExpression => |b| b.position,
            .UnaryExpression => |u| u.position,
            .CallExpression => |c| c.position,
            .MemberExpression => |m| m.position,
            .IndexExpression => |i| i.position,
            .StructLiteral => |s| s.position,
            .TypeExpression => |t| t.position,
            .Identifier => |i| i.position,
            .NumberLiteral => |n| n.position,
            .FloatLiteral => |f| f.position,
            .StringLiteral => |s| s.position,
            .BooleanLiteral => |b| b.position,
            .ArrayLiteral => |a| a.position,
        };
    }

    pub fn get_type(self: Node) Type {
        return switch (self) {
            .BinaryExpression => |b| b.type,
            .UnaryExpression => |u| u.type,
            .CallExpression => |c| c.type,
            .Identifier => |i| i.type,
            .NumberLiteral => |n| n.type,
            .FloatLiteral => |f| f.type,
            .StringLiteral => |s| s.type,
            .BooleanLiteral => |b| b.type,
            .IndexExpression => |i| i.type,
            .MemberExpression => |m| m.type,
            .ExpressionStatement => |e| e.type,
            .ArrayLiteral => |a| a.type,
            .TypeExpression => |t| t.value,
            else => .Void,
        };
    }

    pub fn deinit(self: *Node) void {
        switch (self.*) {
            .ImportStatement => |*i| i.deinit(),
            .FunctionStatement => |*f| f.deinit(),
            .VariableStatement => |*v| v.deinit(),
            .BlockStatement => |*b| b.deinit(),
            .ReturnStatement => |*r| r.deinit(),
            .ExpressionStatement => |*e| e.deinit(),
            .BinaryExpression => |*b| b.deinit(),
            .UnaryExpression => |*u| u.deinit(),
            .StructStatement => |*s| s.deinit(),
            .IfExpression => |*i| i.deinit(),
            .ForStatement => |*f| f.deinit(),
            .WhileStatement => |*w| w.deinit(),
            .CallExpression => |*c| c.deinit(),
            .MemberExpression => |*m| m.deinit(),
            .IndexExpression => |*i| i.deinit(),
            .StructLiteral => |*s| s.deinit(),
            .ArrayLiteral => |*a| a.deinit(),
            .TypeExpression => {},
            .ExternStatement => |*e| {
                switch (e.*) {
                    .Function => |*f| f.deinit(),
                    .Variable => |*v| v.type.deinit(),
                }
            },
            else => {},
        }
    }

    pub fn dump(self: Node, depth: usize) void {
        switch (self) {
            .ImportStatement => |s| s.dump(depth),
            .StructStatement => |s| s.dump(depth),
            .ExternStatement => |e| e.dump(depth),
            .FunctionStatement => |f| f.dump(depth),
            .VariableStatement => |v| v.dump(depth),
            .IfExpression => |i| i.dump(depth),
            .ForStatement => |f| f.dump(depth),
            .WhileStatement => |w| w.dump(depth),
            .ReturnStatement => |r| r.dump(depth),
            .ExpressionStatement => |e| e.dump(depth),
            .BlockStatement => |b| b.dump(depth),
            .BinaryExpression => |b| b.dump(depth),
            .UnaryExpression => |u| u.dump(depth),
            .CallExpression => |c| c.dump(depth),
            .MemberExpression => |m| m.dump(depth),
            .IndexExpression => |i| i.dump(depth),
            .StructLiteral => |s| s.dump(depth),
            .TypeExpression => |t| t.dump(depth),
            .Identifier => |i| i.dump(depth),
            .NumberLiteral => |n| n.dump(depth),
            .FloatLiteral => |f| f.dump(depth),
            .StringLiteral => |s| s.dump(depth),
            .BooleanLiteral => |b| b.dump(depth),
            .ArrayLiteral => |a| a.dump(depth),
        }
    }
};

pub const Visibility = enum { Public, Private };

pub const Identifier = struct {
    position: Position,
    type: Type = Type.unknown(),

    value: []const u8,

    pub fn deinit(self: *Identifier) void {
        _ = self;
    }

    pub fn dump(self: Identifier, depth: usize) void {
        _ = depth;
        std.debug.print("Identifier(value: '{s}')", .{self.value});
    }
};

pub const NumberLiteral = struct {
    position: Position,
    type: Type = Type.unknown(),

    value: []const u8,

    pub fn deinit(self: *NumberLiteral) void {
        _ = self;
    }

    pub fn dump(self: NumberLiteral, depth: usize) void {
        _ = depth;
        std.debug.print("NumberLiteral(value: {s})", .{self.value});
    }
};

pub const FloatLiteral = struct {
    position: Position,
    type: Type = Type.unknown(),

    value: []const u8,

    pub fn deinit(self: *FloatLiteral) void {
        _ = self;
    }

    pub fn dump(self: FloatLiteral, depth: usize) void {
        _ = depth;
        std.debug.print("FloatLiteral(value: {s})", .{self.value});
    }
};

pub const StringLiteral = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    value: []const u8,

    pub fn deinit(self: *StringLiteral) void {
        self.allocator.free(self.value);
    }

    pub fn dump(self: StringLiteral, depth: usize) void {
        _ = depth;
        std.debug.print("StringLiteral(value: \"{s}\")", .{self.value});
    }
};

pub const BooleanLiteral = struct {
    position: Position,
    type: Type = Type.unknown(),

    value: bool,

    pub fn deinit(self: *BooleanLiteral) void {
        _ = self;
    }

    pub fn dump(self: BooleanLiteral, depth: usize) void {
        _ = depth;
        std.debug.print("BooleanLiteral(value: {})", .{self.value});
    }
};

pub const StructLiteralField = struct {
    allocator: std.mem.Allocator,

    position: Position,

    name: Identifier,
    value: *Node,

    pub fn deinit(self: *StructLiteralField) void {
        self.name.deinit();

        const value_pointer = @constCast(self.value);
        value_pointer.deinit();
        self.allocator.destroy(value_pointer);
    }

    pub fn dump(self: StructLiteralField, depth: usize) void {
        std.debug.print("StructLiteralField(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("value: ", .{});
        self.value.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const StructLiteral = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    @"struct": *Node,
    generics: std.ArrayListUnmanaged(GenericArgument),
    fields: std.ArrayListUnmanaged(StructLiteralField),

    pub fn deinit(self: *StructLiteral) void {
        for (self.fields.items) |*field| {
            field.deinit();
        }
        self.fields.deinit(self.allocator);

        for (self.generics.items) |*argument| {
            argument.deinit();
        }
        self.generics.deinit(self.allocator);
    }

    pub fn dump(self: StructLiteral, depth: usize) void {
        std.debug.print("StructLiteral(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.@"struct".dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("fields: [", .{});
        if (self.fields.items.len > 0) {
            std.debug.print("\n", .{});

            for (self.fields.items, 0..) |field, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                field.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ArrayLiteral = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    elements: std.ArrayListUnmanaged(Node),

    pub fn deinit(self: *ArrayLiteral) void {
        for (self.elements.items) |*element| {
            element.deinit();
        }
        self.elements.deinit(self.allocator);

        self.type.deinit(self.allocator);
    }

    pub fn dump(self: ArrayLiteral, depth: usize) void {
        std.debug.print("ArrayLiteral(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("elements: [", .{});
        if (self.elements.items.len > 0) {
            std.debug.print("\n", .{});
            for (self.elements.items, 0..) |element, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                element.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const TypeExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,

    value: Type,

    pub fn deinit(self: *TypeExpression) void {
        self.value.deinit(self.allocator);
    }

    pub fn dump(self: TypeExpression, depth: usize) void {
        std.debug.print("TypeExpression(value: ", .{});
        self.value.dump(depth + 1);
        std.debug.print(")", .{});
    }
};

pub const BinaryExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    left: *Node,
    operator: Token,
    right: *Node,

    pub fn deinit(self: *BinaryExpression) void {
        var left = @constCast(self.left);
        left.deinit();
        self.allocator.destroy(left);

        var right = @constCast(self.right);
        right.deinit();
        self.allocator.destroy(right);
    }

    pub fn dump(self: BinaryExpression, depth: usize) void {
        std.debug.print("BinaryExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("operator: ", .{});
        self.operator.dump();
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("left: ", .{});
        self.left.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("right: ", .{});
        self.right.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const UnaryExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    operator: Token,
    operand: *Node,

    pub fn deinit(self: *UnaryExpression) void {
        self.operand.deinit();
        self.allocator.destroy(self.operand);
    }

    pub fn dump(self: UnaryExpression, depth: usize) void {
        std.debug.print("UnaryExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("operator: ", .{});
        self.operator.dump();
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("operand: ", .{});
        self.operand.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const GenericArgument = struct {
    position: Position,

    name: ?Identifier,
    type: TypeExpression,

    pub fn deinit(self: *GenericArgument) void {
        if (self.name) |*name| {
            name.deinit();
        }

        self.type.deinit();
    }

    pub fn dump(self: GenericArgument, depth: usize) void {
        std.debug.print("GenericArgument(\n", .{});
        type_zig.print_indent(depth + 1);
        if (self.name) |name| {
            std.debug.print("name: ", .{});
            name.dump(depth + 1);
        } else {
            std.debug.print("name: null", .{});
        }
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const Argument = struct {
    allocator: std.mem.Allocator,

    position: Position,

    name: ?Identifier,
    value: *Node,

    pub fn deinit(self: *Argument) void {
        if (self.name) |*name| {
            name.deinit();
        }

        self.value.deinit();
        self.allocator.destroy(self.value);
    }

    pub fn dump(self: Argument, depth: usize) void {
        std.debug.print("Argument(\n", .{});
        type_zig.print_indent(depth + 1);
        if (self.name) |name| {
            std.debug.print("name: ", .{});
            name.dump(depth + 1);
        } else {
            std.debug.print("name: null", .{});
        }
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("value: ", .{});
        self.value.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const CallExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    callee: *Node,
    generics: std.ArrayListUnmanaged(GenericArgument),
    arguments: std.ArrayListUnmanaged(Argument),

    pub fn deinit(self: *CallExpression) void {
        self.callee.deinit();
        self.allocator.destroy(self.callee);

        for (self.generics.items) |*argument| {
            argument.deinit();
        }
        self.generics.deinit(self.allocator);

        for (self.arguments.items) |*argument| {
            argument.deinit();
        }
        self.arguments.deinit(self.allocator);
    }

    pub fn dump(self: CallExpression, depth: usize) void {
        std.debug.print("CallExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("callee: ", .{});
        self.callee.dump(depth + 1);

        if (self.generics.items.len > 0) {
            std.debug.print(",\n", .{});
            type_zig.print_indent(depth + 1);
            std.debug.print("generics: [", .{});

            for (self.generics.items, 0..) |generic, i| {
                if (i > 0) std.debug.print(", ", .{});
                generic.dump(depth + 2);
            }

            std.debug.print("]", .{});
        }

        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("arguments: [", .{});
        if (self.arguments.items.len > 0) {
            std.debug.print("\n", .{});

            for (self.arguments.items, 0..) |argument, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                argument.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const MemberExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    object: *Node,
    property: Identifier,

    pub fn deinit(self: *MemberExpression) void {
        self.object.deinit();
        self.allocator.destroy(self.object);

        self.property.deinit();
    }

    pub fn dump(self: MemberExpression, depth: usize) void {
        std.debug.print("MemberExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("object: ", .{});
        self.object.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("property: ", .{});
        self.property.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const IndexExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    array: *Node,
    index: *Node,

    pub fn deinit(self: *IndexExpression) void {
        self.array.deinit();
        self.allocator.destroy(self.array);

        self.index.deinit();
        self.allocator.destroy(self.index);
    }

    pub fn dump(self: IndexExpression, depth: usize) void {
        std.debug.print("IndexExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("array: ", .{});
        self.array.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("index: ", .{});
        self.index.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ExpressionStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    expression: *Node,

    pub fn deinit(self: *ExpressionStatement) void {
        self.expression.deinit();
        self.allocator.destroy(self.expression);
    }

    pub fn dump(self: ExpressionStatement, depth: usize) void {
        std.debug.print("ExpressionStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("expression: ", .{});
        self.expression.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const BlockStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    scope: *Scope,
    statements: std.ArrayListUnmanaged(Node),

    pub fn deinit(self: *BlockStatement) void {
        self.scope.deinit();
        self.allocator.destroy(self.scope);

        for (self.statements.items) |*statement| {
            statement.deinit();
        }
        self.statements.deinit(self.allocator);
    }

    pub fn dump(self: BlockStatement, depth: usize) void {
        std.debug.print("BlockStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("statements: [", .{});
        if (self.statements.items.len > 0) {
            std.debug.print("\n", .{});
            for (self.statements.items, 0..) |stmt, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                stmt.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("scope: ", .{});
        self.scope.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ImportStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    path: std.ArrayListUnmanaged(Identifier),

    pub fn deinit(self: *ImportStatement) void {
        for (self.path.items) |*identifier| {
            identifier.deinit();
        }
        self.path.deinit(self.allocator);
    }

    pub fn dump(self: ImportStatement, depth: usize) void {
        std.debug.print("ImportStatement(path: [", .{});
        for (self.path.items, 0..) |identifier, i| {
            if (i > 0) std.debug.print(", ", .{});
            identifier.dump(depth + 1);
        }
        std.debug.print("])", .{});
    }
};

pub const ExternFnStatement = struct {
    position: Position,

    visibility: Visibility,

    proto: FunctionProto,

    pub fn deinit(self: *ExternFnStatement) void {
        self.proto.deinit();
    }

    pub fn dump(self: ExternFnStatement, depth: usize) void {
        std.debug.print("ExternFnStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("proto: ", .{});
        self.proto.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ExternVarStatement = struct {
    position: Position,

    visibility: Visibility,

    name: Identifier,
    type: TypeExpression,

    pub fn deinit(self: *ExternVarStatement) void {
        self.name.deinit();
        self.type.deinit();
    }

    pub fn dump(self: ExternVarStatement, depth: usize) void {
        std.debug.print("ExternVarStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ExternStatement = union(enum) {
    Function: ExternFnStatement,
    Variable: ExternVarStatement,

    pub fn position(self: ExternStatement) Position {
        return switch (self) {
            .Function => |f| f.position,
            .Variable => |v| v.position,
        };
    }

    pub fn deinit(self: *ExternStatement) void {
        switch (self.*) {
            .Function => |*function| function.deinit(),
            .Variable => |*variable| variable.deinit(),
        }
    }

    pub fn dump(self: ExternStatement, depth: usize) void {
        switch (self) {
            .Function => |function| function.dump(depth),
            .Variable => |variable| variable.dump(depth),
        }
    }
};

pub const StructStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    visibility: Visibility,

    name: Identifier,
    parent: ?TypeExpression = null,
    generics: std.ArrayListUnmanaged(GenericParameter),
    fields: std.ArrayListUnmanaged(StructStatementField),

    pub fn deinit(self: *StructStatement) void {
        if (self.parent) |*p| p.deinit();

        for (self.generics.items) |*parameter| {
            parameter.deinit();
        }
        self.generics.deinit(self.allocator);

        for (self.fields.items) |*field| {
            field.type.deinit();
        }
        self.fields.deinit(self.allocator);
    }

    pub fn dump(self: StructStatement, depth: usize) void {
        std.debug.print("StructStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});

        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        if (self.parent) |p| {
            std.debug.print(",\n", .{});
            type_zig.print_indent(depth + 1);
            std.debug.print("parent: ", .{});
            p.dump(depth + 1);
        }
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("fields: [", .{});
        if (self.fields.items.len > 0) {
            std.debug.print("\n", .{});

            for (self.fields.items, 0..) |field, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                field.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const StructStatementField = struct {
    position: Position,

    name: Identifier,
    type: TypeExpression,

    pub fn deinit(self: *StructStatementField) void {
        self.name.deinit();
        self.type.deinit();
    }

    pub fn dump(self: StructStatementField, depth: usize) void {
        std.debug.print("StructField(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const Parameter = struct {
    position: Position,

    name: Identifier,
    type: TypeExpression,

    is_variadic: bool = false,

    pub fn deinit(self: *Parameter) void {
        self.name.deinit();
        self.type.deinit();
    }

    pub fn dump(self: Parameter, depth: usize) void {
        std.debug.print("FunctionParameter(\n", .{});
        type_zig.print_indent(depth + 1);
        if (self.is_variadic) std.debug.print("variadic: true,\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const FunctionProto = struct {
    allocator: std.mem.Allocator,

    visibility: Visibility,

    name: Identifier,
    generics: std.ArrayListUnmanaged(GenericParameter),
    parameters: std.ArrayListUnmanaged(Parameter),

    return_type: TypeExpression,

    pub fn deinit(self: *FunctionProto) void {
        for (self.generics.items) |*parameter| {
            parameter.deinit();
        }
        self.generics.deinit(self.allocator);

        for (self.parameters.items) |*parameter| {
            parameter.deinit();
        }
        self.parameters.deinit(self.allocator);

        self.return_type.deinit();
    }

    pub fn dump(self: FunctionProto, depth: usize) void {
        std.debug.print("FunctionProto(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});

        type_zig.print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        if (self.generics.items.len > 0) {
            std.debug.print("\n", .{});

            for (self.generics.items, 0..) |gp, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                gp.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("parameters: [", .{});
        if (self.parameters.items.len > 0) {
            std.debug.print("\n", .{});

            for (self.parameters.items, 0..) |param, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 2);
                param.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            type_zig.print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("return_type: ", .{});
        self.return_type.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const GenericParameter = struct {
    position: Position,

    name: Identifier,
    constraint: ?TypeExpression = null,

    pub fn deinit(self: *GenericParameter) void {
        self.name.deinit();
        if (self.constraint) |*c| c.deinit();
    }

    pub fn dump(self: GenericParameter, depth: usize) void {
        std.debug.print("GenericParameter(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        if (self.constraint) |c| {
            std.debug.print(",\n", .{});
            type_zig.print_indent(depth + 1);
            std.debug.print("constraint: ", .{});
            c.dump(depth + 1);
        }
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const FunctionStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    proto: FunctionProto,
    body: BlockStatement,

    pub fn deinit(self: *FunctionStatement) void {
        self.proto.deinit();
        self.body.deinit();
    }

    pub fn dump(self: FunctionStatement, depth: usize) void {
        std.debug.print("FunctionStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("proto: ", .{});
        self.proto.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("body: ", .{});
        self.body.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const VariableStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    visibility: Visibility,

    name: Identifier,

    type: ?TypeExpression,
    initializer: ?*Node,

    is_mut: bool,

    pub fn deinit(self: *VariableStatement) void {
        if (self.type) |*@"type"| {
            @"type".deinit();
        }

        if (self.initializer) |initializer| {
            initializer.deinit();
            self.allocator.destroy(initializer);
        }
    }

    pub fn dump(self: VariableStatement, depth: usize) void {
        std.debug.print("VariableStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: ", .{});
        self.name.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("is_mut: {},\n", .{self.is_mut});
        if (self.type) |type_expr| {
            type_zig.print_indent(depth + 1);
            std.debug.print("type: ", .{});
            type_expr.dump(depth + 1);
            std.debug.print(",\n", .{});
        }
        if (self.initializer) |init| {
            type_zig.print_indent(depth + 1);
            std.debug.print("initializer: ", .{});
            init.dump(depth + 1);
            std.debug.print(",\n", .{});
        }
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const IfExpression = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    condition: *Node,
    consequence: *Node,
    alternative: ?*Node,

    pub fn deinit(self: *IfExpression) void {
        self.condition.deinit();
        self.allocator.destroy(self.condition);

        self.consequence.deinit();
        self.allocator.destroy(self.consequence);

        if (self.alternative) |alternative| {
            alternative.deinit();
            self.allocator.destroy(alternative);
        }
    }

    pub fn dump(self: IfExpression, depth: usize) void {
        std.debug.print("IfExpression(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("condition: ", .{});
        self.condition.dump(depth + 1);
        std.debug.print(",\n", .{});

        type_zig.print_indent(depth + 1);
        std.debug.print("consequence: ", .{});
        self.consequence.dump(depth + 1);
        std.debug.print(",\n", .{});

        if (self.alternative) |alternative| {
            type_zig.print_indent(depth + 1);
            std.debug.print("alternative: ", .{});
            alternative.dump(depth + 1);
            std.debug.print(",\n", .{});
        }

        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ForStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    init: ?*Node,
    condition: ?*Node,
    update: ?*Node,

    body: BlockStatement,

    pub fn deinit(self: *ForStatement) void {
        if (self.init) |init| {
            init.deinit();
            self.allocator.destroy(init);
        }

        if (self.condition) |condition| {
            condition.deinit();
            self.allocator.destroy(condition);
        }

        if (self.update) |update| {
            update.deinit();
            self.allocator.destroy(update);
        }

        self.body.deinit();
    }

    pub fn dump(self: ForStatement, depth: usize) void {
        std.debug.print("ForStatement(\n", .{});
        if (self.init) |i| {
            type_zig.print_indent(depth + 1);
            std.debug.print("init: ", .{});
            i.dump(depth + 1);
            std.debug.print(",\n", .{});
        }
        if (self.condition) |c| {
            type_zig.print_indent(depth + 1);
            std.debug.print("condition: ", .{});
            c.dump(depth + 1);
            std.debug.print(",\n", .{});
        }
        if (self.update) |u| {
            type_zig.print_indent(depth + 1);
            std.debug.print("update: ", .{});
            u.dump(depth + 1);
            std.debug.print(",\n", .{});
        }
        type_zig.print_indent(depth + 1);
        std.debug.print("body: ", .{});
        self.body.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const WhileStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,

    condition: *Node,
    body: BlockStatement,

    pub fn deinit(self: *WhileStatement) void {
        self.condition.deinit();
        self.allocator.destroy(self.condition);

        self.body.deinit();
    }

    pub fn dump(self: WhileStatement, depth: usize) void {
        std.debug.print("WhileStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("condition: ", .{});
        self.condition.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("body: ", .{});
        self.body.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};

pub const ReturnStatement = struct {
    allocator: std.mem.Allocator,

    position: Position,
    type: Type = Type.unknown(),

    value: *Node,

    pub fn deinit(self: *ReturnStatement) void {
        self.value.deinit();
        self.allocator.destroy(self.value);
    }

    pub fn dump(self: ReturnStatement, depth: usize) void {
        std.debug.print("ReturnStatement(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("value: ", .{});
        self.value.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};
