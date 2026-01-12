const std = @import("std");

const node = @import("../ast/node.zig");
const memory = @import("../utils/memory.zig");

pub fn print_indent(depth: usize) void {
    for (0..depth) |_| {
        std.debug.print("  ", .{});
    }
}

pub const IntegerType = struct {
    is_unsigned: bool,
    size: u16,

    pub fn dump(self: IntegerType, depth: usize) void {
        _ = depth;
        std.debug.print("IntegerType(is_unsigned: {}, size: {d})", .{ self.is_unsigned, self.size });
    }

    pub fn to_string(self: IntegerType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}{d}", .{ if (self.is_unsigned) "u" else "i", self.size }) catch "int";
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
        return std.fmt.bufPrint(&buf, "f{d}", .{self.size}) catch "float";
    }
};

pub const ArrayType = struct {
    child: *const Type,
    size: ?usize,

    pub fn dump(self: ArrayType, depth: usize) void {
        std.debug.print("ArrayType(child: ", .{});
        self.child.dump(depth);
        if (self.size) |s| {
            std.debug.print(", size: {d})", .{s});
        } else {
            std.debug.print(")", .{});
        }
    }

    pub fn to_string(self: ArrayType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "array[{s}]", .{self.child.to_string()}) catch "array";
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
        return std.fmt.bufPrint(&buf, "pointer[{s}]", .{self.child.to_string()}) catch "pointer";
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type: *const Type,

    pub fn dump(self: StructFieldType, depth: usize) void {
        std.debug.print("StructField(name: '{s}', type: ", .{self.name});
        self.type.dump(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: StructFieldType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.type.to_string() }) catch "struct field";
    }
};

pub const StructType = struct {
    generics: []const []const u8 = &[_][]const u8{},
    fields: []const StructFieldType = &[_]StructFieldType{},

    pub fn dump(self: StructType, depth: usize) void {
        std.debug.print("StructType(fields: [", .{});
        if (self.fields.len > 0) {
            std.debug.print("\n", .{});

            for (self.fields, 0..) |field, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 1);
                field.dump(depth + 1);
            }

            std.debug.print("\n", .{});
            print_indent(depth);
        }
        std.debug.print("])", .{});
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

    pub fn clone(self: StructType, allocator: std.mem.Allocator) !StructType {
        var fields = try allocator.alloc(StructFieldType, self.fields.len);
        for (self.fields, 0..) |field, i| {
            fields[i] = StructFieldType{
                .name = try allocator.dupe(u8, field.name),
                .type = try memory.create(allocator, Type, try field.type.clone(allocator)),
            };
        }

        var generics = try allocator.alloc([]const u8, self.generics.len);
        for (self.generics, 0..) |generic, i| {
            generics[i] = try allocator.dupe(u8, generic);
        }

        return StructType{
            .fields = fields,
            .generics = generics,
        };
    }

    pub fn to_string(self: StructType) []const u8 {
        _ = self;
        return "struct";
    }
};

pub const FunctionParameterType = struct {
    name: []const u8,
    type: *const Type,

    pub fn dump(self: FunctionParameterType, depth: usize) void {
        std.debug.print("FunctionParameterType(name: '{s}', type: ", .{self.name});
        self.type.dump(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: FunctionParameterType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.type.to_string() }) catch "parameter";
    }
};

pub const GenericParameterType = struct {
    name: []const u8,
    constraint: ?*const Type = null,
};

pub const FunctionType = struct {
    parameters: []const FunctionParameterType,
    generics: []const GenericParameterType,
    return_type: *const Type,

    is_variadic: bool = false,

    pub fn dump(self: FunctionType, depth: usize) void {
        std.debug.print("FunctionType(parameters: [", .{});
        if (self.parameters.len > 0) {
            std.debug.print("\n", .{});

            for (self.parameters, 0..) |parameter, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 1);
                parameter.dump(depth + 1);
            }

            std.debug.print("\n", .{});
            print_indent(depth);
        }
        std.debug.print("]", .{});
        std.debug.print(", return_type: ", .{});
        self.return_type.dump(depth);
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

    pub fn clone(self: FunctionType, allocator: std.mem.Allocator) !FunctionType {
        var parameters = try allocator.alloc(FunctionParameterType, self.parameters.len);
        for (self.parameters, 0..) |parameter, i| {
            parameters[i] = FunctionParameterType{
                .name = try allocator.dupe(u8, parameter.name),
                .type = try memory.create(allocator, Type, try parameter.type.clone(allocator)),
            };
        }

        var generics = try allocator.alloc(GenericParameterType, self.generics.len);
        for (self.generics, 0..) |generic, i| {
            generics[i] = GenericParameterType{
                .name = try allocator.dupe(u8, generic.name),
                .constraint = if (generic.constraint) |c| try memory.create(allocator, Type, try c.clone(allocator)) else null,
            };
        }

        return FunctionType{
            .parameters = parameters,
            .generics = generics,
            .return_type = try memory.create(allocator, Type, try self.return_type.clone(allocator)),
            .is_variadic = self.is_variadic,
        };
    }

    pub fn to_string(self: FunctionType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "function( -> {s})", .{self.return_type.to_string()}) catch "function";
    }
};

pub const ClassFieldType = struct {
    name: []const u8,
    type: *const Type,

    pub fn dump(self: ClassFieldType, depth: usize) void {
        std.debug.print("ClassFieldType(name: '{s}', type: ", .{self.name});
        self.type.dump(depth);
        std.debug.print(")", .{});
    }

    pub fn to_string(self: ClassFieldType) []const u8 {
        var buf: [64]u8 = undefined;
        return std.fmt.bufPrint(&buf, "{s}: {s}", .{ self.name, self.type.to_string() }) catch "class field";
    }
};

pub const ClassMethodType = struct {
    name: []const u8,
    type: FunctionType,

    pub fn dump(self: ClassMethodType, depth: usize) void {
        std.debug.print("ClassMethodType(name: '{s}', type: ", .{self.name});
        self.type.dump(depth);
        std.debug.print(")", .{});
    }
};

pub const ClassType = struct {
    generics: []const []const u8 = &[_][]const u8{},
    fields: []const ClassFieldType = &[_]ClassFieldType{},
    methods: []const ClassMethodType = &[_]ClassMethodType{},

    pub fn dump(self: ClassType, depth: usize) void {
        std.debug.print("ClassType(\n", .{});
        print_indent(depth + 1);
        std.debug.print("generics: [", .{});
        for (self.generics, 0..) |generic, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("'{s}'", .{generic});
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
        std.debug.print("],\n", .{});

        print_indent(depth + 1);
        std.debug.print("methods: [", .{});
        if (self.methods.len > 0) {
            std.debug.print("\n", .{});

            for (self.methods, 0..) |method, i| {
                if (i > 0) std.debug.print(",\n", .{});
                print_indent(depth + 2);
                method.dump(depth + 2);
            }

            std.debug.print("\n", .{});
            print_indent(depth + 1);
        }
        std.debug.print("])", .{});
    }

    pub fn deinit(self: ClassType, allocator: std.mem.Allocator) void {
        if (self.fields.len > 0) {
            for (self.fields) |field| {
                var field_type = @constCast(field.type);
                field_type.deinit(allocator);
                allocator.destroy(field_type);
            }
            allocator.free(self.fields);
        }

        if (self.methods.len > 0) {
            for (self.methods) |method| {
                method.type.deinit(allocator);
            }
            allocator.free(self.methods);
        }

        if (self.generics.len > 0) {
            allocator.free(self.generics);
        }
    }

    pub fn clone(self: ClassType, allocator: std.mem.Allocator) !ClassType {
        var fields = try allocator.alloc(ClassFieldType, self.fields.len);
        for (self.fields, 0..) |field, i| {
            fields[i] = ClassFieldType{
                .name = try allocator.dupe(u8, field.name),
                .type = try memory.create(allocator, Type, try field.type.clone(allocator)),
            };
        }

        var methods = try allocator.alloc(ClassMethodType, self.methods.len);
        for (self.methods, 0..) |method, i| {
            methods[i] = ClassMethodType{
                .name = try allocator.dupe(u8, method.name),
                .type = try method.type.clone(allocator),
            };
        }

        var generics = try allocator.alloc([]const u8, self.generics.len);
        for (self.generics, 0..) |generic, i| {
            generics[i] = try allocator.dupe(u8, generic);
        }

        return ClassType{
            .fields = fields,
            .methods = methods,
            .generics = generics,
        };
    }

    pub fn to_string(self: ClassType) []const u8 {
        _ = self;
        return "class";
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

    pub fn dump(self: NamedType, depth: usize) void {
        _ = depth;
        std.debug.print("NamedType(name: '{s}')", .{self.name});
    }

    pub fn to_string(self: NamedType) []const u8 {
        return self.name;
    }
};

pub const Type = union(enum) {
    Unknown: void,
    String: void,
    Boolean: void,
    Integer: IntegerType,
    Float: FloatType,
    Array: ArrayType,
    Pointer: PointerType,
    Void: void,
    Struct: StructType,
    Function: FunctionType,
    Class: ClassType,
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
            .Class => |c| c.deinit(allocator),
            else => {},
        }
    }

    pub fn clone(self: Type, allocator: std.mem.Allocator) std.mem.Allocator.Error!Type {
        return switch (self) {
            .Unknown => .Unknown,
            .String => .String,
            .Boolean => .Boolean,
            .Integer => |i| Type{ .Integer = i },
            .Float => |f| Type{ .Float = f },
            .Void => .Void,
            .Array => |a| Type{ .Array = ArrayType{
                .child = try memory.create(allocator, Type, try a.child.*.clone(allocator)),
                .size = a.size,
            } },
            .Pointer => |p| Type{ .Pointer = PointerType{
                .child = try memory.create(allocator, Type, try p.child.*.clone(allocator)),
            } },
            .Function => |f| Type{ .Function = try f.clone(allocator) },
            .Struct => |s| Type{ .Struct = try s.clone(allocator) },
            .Class => |c| Type{ .Class = try c.clone(allocator) },
            .Named => |n| Type{ .Named = n },
        };
    }

    fn printIndent(depth: usize) void {
        for (0..depth) |_| {
            std.debug.print("  ", .{});
        }
    }

    pub fn dump(self: Type, depth: usize) void {
        switch (self) {
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
            .Function => |function| function.dump(depth),
            .Class => |class| class.dump(depth),
            .Named => |named| named.dump(depth),
        }
    }

    pub fn to_string(self: Type) []const u8 {
        switch (self) {
            .Unknown => return "unknown",
            .String => return "string",
            .Boolean => return "bool",
            .Void => return "void",
            .Integer => |integer| return integer.to_string(),
            .Float => |float| return float.to_string(),
            .Array => |array| return array.to_string(),
            .Pointer => |pointer| return pointer.to_string(),
            .Struct => |structure| return structure.to_string(),
            .Function => |function| return function.to_string(),
            .Class => |class| return class.to_string(),
            .Named => |named| return named.to_string(),
        }
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
};
