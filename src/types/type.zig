const std = @import("std");

const node = @import("../ast/node.zig");
const memory = @import("../utils/memory.zig");

pub fn print_indent(depth: usize) void {
    for (0..depth) |_| {
        std.debug.print("  ", .{});
    }
}

pub const GenericParameterType = struct {
    name: []const u8,
    constraint: ?*const Type = null,

    pub fn dump(self: GenericParameterType, depth: usize) void {
        std.debug.print("GenericParameterType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        print_indent(depth + 1);
        std.debug.print("constraint: ", .{});
        if (self.constraint) |constraint| {
            constraint.dump(depth + 1);
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print("\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: GenericParameterType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.constraint }) catch self.name;
    }
};

pub const GenericArgumentType = struct {
    type: *const Type,

    pub fn dump(self: GenericArgumentType, depth: usize) void {
        std.debug.print("GenericArgumentType(type: ", .{});
        self.type.dump(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: GenericArgumentType) []const u8 {
        return self.type.to_string();
    }
};

pub const IntegerType = struct {
    is_unsigned: bool,
    size: u16,

    pub fn dump(self: IntegerType, depth: usize) void {
        std.debug.print("IntegerType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("is_unsigned: {},\n", .{self.is_unsigned});
        print_indent(depth + 1);
        std.debug.print("size: {d}\n", .{self.size});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: IntegerType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}{d}", .{ if (self.is_unsigned) "u" else "i", self.size }) catch "i32";
    }
};

pub const FloatType = struct {
    size: u16,

    pub fn dump(self: FloatType, depth: usize) void {
        _ = depth;
        std.debug.print("FloatType(size: {d})", .{self.size});
    }

    pub fn to_string(self: FloatType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "f{d}", .{self.size}) catch "f64";
    }
};

pub const ArrayType = struct {
    child: *const Type,
    size: ?usize,

    pub fn dump(self: ArrayType, depth: usize) void {
        std.debug.print("ArrayType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("child: ", .{});
        self.child.dump(depth + 1);
        if (self.size) |s| {
            std.debug.print(",\n", .{});
            print_indent(depth + 1);
            std.debug.print("size: {d}\n", .{s});
        } else {
            std.debug.print("\n", .{});
        }
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: ArrayType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "[{s}]", .{self.child.to_string()}) catch "[]";
    }
};

pub const PointerType = struct {
    child: *const Type,

    pub fn dump(self: PointerType, depth: usize) void {
        std.debug.print("PointerType(child: ", .{});
        self.child.dump(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: PointerType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "*{s}", .{self.child.to_string()}) catch "*";
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type: *const Type,

    pub fn dump(self: StructFieldType, depth: usize) void {
        std.debug.print("StructField(\n", .{});
        print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: StructFieldType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.type.to_string() }) catch "struct field";
    }
};

pub const StructType = struct {
    name: []const u8,
    generics: []const GenericParameterType,
    fields: []const StructFieldType,

    pub fn dump(self: StructType, depth: usize) void {
        std.debug.print("StructType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});

        print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        if (self.generics.len > 0) {
            std.debug.print("\n", .{});
            for (self.generics, 0..) |generic, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                generic.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        print_indent(depth + 1);
        std.debug.print("fields: [", .{});
        if (self.fields.len > 0) {
            std.debug.print("\n", .{});
            for (self.fields, 0..) |field, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                field.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: StructType, allocator: std.mem.Allocator) void {
        if (self.fields.len > 0) {
            for (self.fields) |field| {
                var field_type = @constCast(field.type);
                field_type.deinit(allocator);
                allocator.destroy(field_type);
            }
            allocator.free(self.fields);
        }

        if (self.generics.len > 0) {
            allocator.free(self.generics);
        }
    }

    pub fn to_string(self: StructType) []const u8 {
        return self.name;
    }
};

pub const StructLiteralType = struct {
    @"struct": *const StructType,
    generics: []const GenericArgumentType,

    pub fn dump(self: StructLiteralType, depth: usize) void {
        std.debug.print("StructLiteralType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.@"struct".dump(depth + 1);
        std.debug.print(",\n", .{});
        print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        if (self.generics.len > 0) {
            std.debug.print("\n", .{});
            for (self.generics, 0..) |generic, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                generic.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: StructLiteralType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}", .{self.@"struct".to_string()}) catch self.@"struct".to_string();
    }
};

pub const FunctionParameterType = struct {
    name: []const u8,
    type: *const Type,

    pub fn dump(self: FunctionParameterType, depth: usize) void {
        std.debug.print("FunctionParameterType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: FunctionParameterType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.type.to_string() }) catch self.name;
    }
};

pub const FunctionType = struct {
    parameters: []const FunctionParameterType,
    generics: []const GenericParameterType,
    return_type: *const Type,

    is_variadic: bool = false,
    is_operator: bool = false,

    pub fn dump(self: FunctionType, depth: usize) void {
        std.debug.print("FunctionType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("parameters: [", .{});
        if (self.parameters.len > 0) {
            std.debug.print("\n", .{});
            for (self.parameters, 0..) |parameter, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                parameter.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        if (self.generics.len > 0) {
            std.debug.print("\n", .{});
            for (self.generics, 0..) |generic, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                generic.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("],\n", .{});

        print_indent(depth + 1);
        std.debug.print("return_type: ", .{});
        self.return_type.dump(depth + 1);
        std.debug.print(",\n", .{});

        print_indent(depth + 1);
        std.debug.print("is_variadic: {},\n", .{self.is_variadic});

        print_indent(depth + 1);
        std.debug.print("is_operator: {}\n", .{self.is_operator});

        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: FunctionType, allocator: std.mem.Allocator) void {
        if (self.parameters.len > 0) {
            for (self.parameters) |parameter| {
                var param_type = @constCast(parameter.type);
                param_type.deinit(allocator);
                allocator.destroy(param_type);
            }
            allocator.free(self.parameters);
        }

        if (self.generics.len > 0) {
            allocator.free(self.generics);
        }

        var return_type = @constCast(self.return_type);
        return_type.deinit(allocator);
        allocator.destroy(return_type);
    }

    pub fn to_string(self: FunctionType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "function( -> {s})", .{self.return_type.to_string()}) catch "function";
    }
};

pub const UnknownType = struct {
    name: ?[]const u8,

    pub fn dump(self: UnknownType, depth: usize) void {
        _ = depth;
        std.debug.print("UnknownType(name: '{s}')", .{self.name orelse ""});
    }

    pub fn to_string(self: UnknownType) []const u8 {
        return self.name orelse "unknown";
    }
};

pub const NamedType = struct {
    name: []const u8,
    generics: []const GenericArgumentType = &[_]GenericArgumentType{},

    pub fn dump(self: NamedType, depth: usize) void {
        std.debug.print("NamedType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        if (self.generics.len > 0) {
            std.debug.print("\n", .{});
            for (self.generics, 0..) |generic, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                generic.type.dump(depth + 2);
            }
            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("]\n", .{});
        print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: *NamedType, allocator: std.mem.Allocator) void {
        for (self.generics) |*generic| {
            var generic_type = @constCast(generic.type);
            generic_type.deinit(allocator);
            allocator.destroy(generic_type);
        }
        allocator.free(self.generics);
    }

    pub fn to_string(self: NamedType) []const u8 {
        return self.name;
    }
};

pub const Type = union(enum) {
    Unknown: void,
    String: void,
    Boolean: void,
    Any: void,
    Integer: IntegerType,
    Float: FloatType,
    Array: ArrayType,
    Pointer: PointerType,
    Void: void,
    Struct: StructType,
    StructLiteral: StructLiteralType,
    Function: FunctionType,
    Named: NamedType,

    pub fn deinit(self: *Type, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Array => |a| {
                var child = @constCast(a.child);
                child.deinit(allocator);
                allocator.destroy(child);
            },
            .Pointer => |p| {
                var child = @constCast(p.child);
                child.deinit(allocator);
                allocator.destroy(child);
            },
            .Function => |f| f.deinit(allocator),
            .Struct => |s| s.deinit(allocator),
            .Named => |*n| n.deinit(allocator),
            else => {},
        }
    }

    fn printIndent(depth: usize) void {
        for (0..depth) |_| {
            std.debug.print("  ", .{});
        }
    }

    pub fn dump(self: Type, depth: usize) void {
        switch (self) {
            .Any => {
                std.debug.print("AnyType()", .{});
            },
            .Unknown => {
                std.debug.print("UnknownType()", .{});
            },
            .String => {
                std.debug.print("StringType()", .{});
            },
            .Boolean => {
                std.debug.print("BooleanType()", .{});
            },
            .Void => {
                std.debug.print("VoidType()", .{});
            },
            .Integer => |integer| integer.dump(depth),
            .Float => |float_type| float_type.dump(depth),
            .Array => |array| array.dump(depth),
            .Pointer => |pointer| pointer.dump(depth),
            .Struct => |structure| structure.dump(depth),
            .StructLiteral => |structure_reference| structure_reference.dump(depth),
            .Function => |function| function.dump(depth),
            .Named => |named| named.dump(depth),
        }
    }

    pub fn to_string(self: Type) []const u8 {
        switch (self) {
            .Any => return "any",
            .Unknown => return "unknown",
            .String => return "string",
            .Boolean => return "bool",
            .Void => return "void",
            .Integer => |integer| return integer.to_string(),
            .Float => |float| return float.to_string(),
            .Array => |array| return array.to_string(),
            .Pointer => |pointer| return pointer.to_string(),
            .Struct => |@"struct"| return @"struct".to_string(),
            .StructLiteral => |struct_literal| return struct_literal.to_string(),
            .Function => |function| return function.to_string(),
            .Named => |named| return named.to_string(),
        }
    }

    pub fn equal(a: Type, b: Type) bool {
        if (@as(std.meta.Tag(Type), a) != @as(std.meta.Tag(Type), b)) return false;

        return switch (a) {
            .Integer => |ai| ai.size == b.Integer.size and ai.is_unsigned == b.Integer.is_unsigned,
            .Float => |af| af.size == b.Float.size,
            else => true,
        };
    }

    pub fn @"void"() Type {
        return Type{ .Void = {} };
    }

    pub fn unknown() Type {
        return Type{ .Unknown = {} };
    }

    pub fn is_unknown(self: Type) bool {
        return self == .Unknown;
    }

    pub fn is_any(self: Type) bool {
        return self == .Any;
    }

    pub fn is_numeric(self: Type) bool {
        return self == .Integer or self == .Float or self.is_unknown() or self == .Named;
    }

    pub fn is_compatible(expected: Type, actual: Type) bool {
        if (expected.is_any() or actual.is_any()) {
            return true;
        }

        if (expected.is_unknown() or actual.is_unknown()) {
            return true;
        }

        if (expected == .StructLiteral and actual == .Struct) {
            return Type.is_compatible(Type{ .Struct = expected.StructLiteral.@"struct".* }, actual);
        }

        if (expected == .Struct and actual == .StructLiteral) {
            return Type.is_compatible(expected, Type{ .Struct = actual.StructLiteral.@"struct".* });
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
            .Integer => actual.Integer.size == expected.Integer.size and actual.Integer.is_unsigned == expected.Integer.is_unsigned,
            .Float => actual.Float.size == expected.Float.size,
            .String => true,
            .Boolean => true,
            .Void => true,
            .Pointer => |pointer| {
                if (actual != .Pointer) return false;

                if (pointer.child.* == .Void) return true;

                return Type.is_compatible(pointer.child.*, actual.Pointer.child.*);
            },
            .Array => |array| {
                if (actual != .Array) return false;

                return Type.is_compatible(array.child.*, actual.Array.child.*);
            },
            .Struct => |@"struct"| {
                if (actual != .Struct) return false;

                if (@"struct".fields.len != actual.Struct.fields.len) return false;

                for (@"struct".fields, 0..) |field, i| {
                    if (!std.mem.eql(u8, field.name, actual.Struct.fields[i].name)) return false;
                    if (!Type.is_compatible(field.type.*, actual.Struct.fields[i].type.*)) return false;
                }

                return true;
            },
            .StructLiteral => |struct_literal| {
                if (actual != .StructLiteral) return false;

                if (!Type.is_compatible(Type{ .Struct = struct_literal.@"struct".* }, Type{ .Struct = actual.StructLiteral.@"struct".* })) return false;

                if (struct_literal.generics.len != actual.StructLiteral.generics.len) return false;

                for (struct_literal.generics, 0..) |generic, i| {
                    if (!Type.is_compatible(generic.type.*, actual.StructLiteral.generics[i].type.*)) return false;
                }

                return true;
            },
            .Function => |function| {
                if (actual != .Function) return false;

                if (!Type.is_compatible(function.return_type.*, actual.Function.return_type.*)) return false;

                if (function.parameters.len != actual.Function.parameters.len) return false;

                for (function.parameters, 0..) |p, i| {
                    if (!Type.is_compatible(p.type.*, actual.Function.parameters[i].type.*)) return false;
                }

                return true;
            },
            .Unknown => false,
            .Named => true,
            .Any => true,
        };
    }
};
