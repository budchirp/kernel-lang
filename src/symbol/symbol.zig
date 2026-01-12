const std = @import("std");
const node_zig = @import("../ast/node.zig");
const type_zig = @import("../types/type.zig");

const Visibility = node_zig.Visibility;
const Type = type_zig.Type;
const FunctionType = type_zig.FunctionType;

pub const TypeSymbol = struct {
    name: []const u8,

    visibility: Visibility,

    type: Type,

    pub fn dump(self: TypeSymbol, depth: usize) void {
        std.debug.print("TypeSymbol(name: '{s}', visibility: {s}, type: \n", .{ self.name, @tagName(self.visibility) });
        self.type.dump(depth + 1);
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

    is_operator: bool,

    pub fn dump(self: FunctionSymbol, depth: usize) void {
        std.debug.print("FunctionSymbol(name: '{s}', visibility: {s}, is_operator: {}, type: \n", .{ self.name, @tagName(self.visibility), self.is_operator });
        self.type.dump(depth + 1);
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
        std.debug.print("type: ", .{});
        self.type.dump(depth + 1);
        std.debug.print(",\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("is_mutable: {}, visibility: {s}\n", .{ self.is_mutable, @tagName(self.visibility) });
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
