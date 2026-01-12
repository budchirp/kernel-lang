const std = @import("std");

const symbol_zig = @import("symbol.zig");
const type_zig = @import("../types/type.zig");

const Symbol = symbol_zig.Symbol;
const TypeSymbol = symbol_zig.TypeSymbol;
const FunctionSymbol = symbol_zig.FunctionSymbol;
const VariableSymbol = symbol_zig.VariableSymbol;
const Type = type_zig.Type;

pub const ScopeType = enum {
    Function,
    Block,
    Global,
};

pub const FunctionOverloads = std.ArrayListUnmanaged(FunctionSymbol);

pub const Scope = struct {
    allocator: std.mem.Allocator,

    name: []const u8,
    parent: ?*Scope,

    type: ScopeType,

    function: ?FunctionSymbol,

    functions: std.StringHashMapUnmanaged(FunctionOverloads),
    types: std.StringHashMapUnmanaged(TypeSymbol),
    variables: std.StringHashMapUnmanaged(VariableSymbol),

    pub fn init(allocator: std.mem.Allocator, name: []const u8, parent: ?*Scope) !Scope {
        var _name: []const u8 = undefined;
        if (parent) |scope| {
            _name = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ scope.name, name });
        } else {
            _name = try allocator.dupe(u8, name);
        }

        return Scope{
            .allocator = allocator,
            .name = _name,
            .parent = parent,
            .type = .Block,
            .function = null,
            .functions = std.StringHashMapUnmanaged(FunctionOverloads){},
            .types = std.StringHashMapUnmanaged(TypeSymbol){},
            .variables = std.StringHashMapUnmanaged(VariableSymbol){},
        };
    }

    pub fn deinit(self: *Scope) void {
        self.allocator.free(self.name);

        var func_iter = self.functions.iterator();
        while (func_iter.next()) |entry| {
            for (entry.value_ptr.items) |*func| {
                func.deinit(self.allocator);
            }
            entry.value_ptr.deinit(self.allocator);
        }
        self.functions.deinit(self.allocator);

        self.types.deinit(self.allocator);

        var var_iter = self.variables.iterator();
        while (var_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.variables.deinit(self.allocator);
    }

    pub fn is_global(self: *Scope) bool {
        return self.parent == null;
    }

    fn signatures_equal(a: FunctionSymbol, b: FunctionSymbol) bool {
        if (a.type.parameters.len != b.type.parameters.len) return false;
        if (a.type.generics.len != b.type.generics.len) return false;

        for (a.type.parameters, b.type.parameters) |pa, pb| {
            if (!types_equal(pa.type.*, pb.type.*)) return false;
        }

        return true;
    }

    fn types_equal(a: Type, b: Type) bool {
        if (@as(std.meta.Tag(Type), a) != @as(std.meta.Tag(Type), b)) return false;

        return switch (a) {
            .Integer => |ai| ai.size == b.Integer.size and ai.is_unsigned == b.Integer.is_unsigned,
            .Float => |af| af.size == b.Float.size,
            .Pointer => |ap| types_equal(ap.child.*, b.Pointer.child.*),
            .Array => |aa| types_equal(aa.child.*, b.Array.child.*),
            .Named => |an| std.mem.eql(u8, an.name, b.Named.name),
            else => true,
        };
    }

    pub fn add_function(self: *Scope, function: FunctionSymbol) !void {
        if (self.functions.getPtr(function.name)) |overloads| {
            for (overloads.items) |existing| {
                if (signatures_equal(existing, function)) {
                    return error.DuplicateFunction;
                }
            }
            try overloads.append(self.allocator, function);
        } else {
            var overloads = FunctionOverloads{};
            try overloads.append(self.allocator, function);
            try self.functions.put(self.allocator, function.name, overloads);
        }
    }

    pub fn lookup_function(self: *Scope, name: []const u8) ?FunctionSymbol {
        if (self.functions.get(name)) |overloads| {
            if (overloads.items.len > 0) return overloads.items[0];
        }

        if (self.parent) |parent| {
            return parent.lookup_function(name);
        }

        return null;
    }

    pub fn lookup_function_overloads(self: *Scope, name: []const u8) ?[]FunctionSymbol {
        if (self.functions.get(name)) |overloads| {
            return overloads.items;
        }

        if (self.parent) |parent| {
            return parent.lookup_function_overloads(name);
        }

        return null;
    }

    pub fn lookup_type(self: *Scope, name: []const u8) ?TypeSymbol {
        if (self.types.get(name)) |@"type"| {
            return @"type";
        }

        if (self.parent) |parent| {
            return parent.lookup_type(name);
        }

        return null;
    }

    pub fn add_type(self: *Scope, @"type": TypeSymbol) !void {
        try self.types.put(self.allocator, @"type".name, @"type");
    }

    pub fn lookup_variable(self: *const Scope, name: []const u8) ?VariableSymbol {
        if (self.variables.get(name)) |symbol| {
            return symbol;
        }

        if (self.parent) |parent| {
            return parent.lookup_variable(name);
        }

        return null;
    }

    pub fn add_variable(self: *Scope, symbol: VariableSymbol) !void {
        try self.variables.put(self.allocator, symbol.name, symbol);
    }

    pub fn dump(self: Scope, depth: usize) void {
        std.debug.print("Scope(\n", .{});
        type_zig.print_indent(depth + 1);
        std.debug.print("name: '{s}',\n", .{self.name});
        type_zig.print_indent(depth + 1);
        std.debug.print("variables: [", .{});
        if (self.variables.size > 0) {
            type_zig.print_indent(depth + 1);
            std.debug.print("\n", .{});

            var var_iter = self.variables.iterator();
            var i: usize = 0;
            while (var_iter.next()) |entry| {
                if (i > 0) std.debug.print(",\n", .{});
                entry.value_ptr.dump(depth + 2);
                i += 1;
            }
        }

        std.debug.print("]\n", .{});
        type_zig.print_indent(depth);
        std.debug.print(")", .{});
    }
};
