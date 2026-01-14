const std = @import("std");
const type_zig = @import("../types/type.zig");

const Type = type_zig.Type;
const FunctionType = type_zig.FunctionType;

pub const Visibility = enum { Public, Private };

pub const TypeSymbol = struct {
    name: []const u8,

    visibility: Visibility,

    type: Type,

    pub fn dump(self: TypeSymbol, depth: usize) void {
        std.debug.print("TypeSymbol(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: *TypeSymbol, allocator: std.mem.Allocator) void {
        self.type.deinit(allocator);
    }
};

pub const FunctionSymbol = struct {
    name: []const u8,

    visibility: Visibility,

    type: FunctionType,

    pub fn dump(self: FunctionSymbol, depth: usize) void {
        std.debug.print("FunctionSymbol(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: *FunctionSymbol, allocator: std.mem.Allocator) void {
        self.type.deinit(allocator);
    }
};

pub const VariableSymbol = struct {
    name: []const u8,

    visibility: Visibility,

    type: Type,

    is_mutable: bool,

    pub fn dump(self: VariableSymbol, depth: usize) void {
        std.debug.print("VariableSymbol(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        type_zig.print_indent(depth + 1);
        std.debug.print("visibility: {s},\n", .{@tagName(self.visibility)});
        type_zig.print_indent(depth + 1);
        std.debug.print("is_mutable: {},\n", .{self.is_mutable});
        type_zig.print_indent(depth + 1);
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print("\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }

    pub fn deinit(self: *VariableSymbol, allocator: std.mem.Allocator) void {
        self.type.deinit(allocator);
    }
};

pub const Symbol = union(enum) {
    Type: TypeSymbol,
    Function: FunctionSymbol,
    Variable: VariableSymbol,

    pub fn dump(self: Symbol, depth: usize) void {
        switch (self) {
            .Type => |type_symbol| type_symbol.dump(depth),
            .Function => |function_symbol| function_symbol.dump(depth),
            .Variable => |variable_symbol| variable_symbol.dump(depth),
        }
    }

    pub fn deinit(self: *Symbol, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Type => |*type_symbol| type_symbol.deinit(allocator),
            .Function => |*function_symbol| function_symbol.deinit(allocator),
            .Variable => |*variable_symbol| variable_symbol.deinit(allocator),
        }
    }

    pub fn name(self: Symbol) []const u8 {
        return switch (self) {
            .Type => |type_symbol| type_symbol.name,
            .Function => |function_symbol| function_symbol.name,
            .Variable => |variable_symbol| variable_symbol.name,
        };
    }
};
