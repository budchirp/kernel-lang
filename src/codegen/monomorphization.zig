const std = @import("std");

const node_zig = @import("../ast/node.zig");
const type_zig = @import("../types/type.zig");

const Type = type_zig.Type;
const FunctionStatement = node_zig.FunctionStatement;

pub const MonoKey = struct {
    name: []const u8,
    types: []const Type,

    pub fn hash(self: MonoKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(self.name);

        for (self.types) |@"type"| {
            hasher.update(@"type".to_string());
        }

        return hasher.final();
    }

    pub fn eql(a: MonoKey, b: MonoKey) bool {
        if (!std.mem.eql(u8, a.name, b.name)) return false;
        if (a.types.len != b.types.len) return false;

        for (a.types, b.types) |type_a, type_b| {
            if (!std.mem.eql(u8, type_a.to_string(), type_b.to_string())) return false;
        }

        return true;
    }
};

pub const MonoKeyContext = struct {
    pub fn hash(_: MonoKeyContext, key: MonoKey) u64 {
        return key.hash();
    }

    pub fn eql(_: MonoKeyContext, a: MonoKey, b: MonoKey) bool {
        return MonoKey.eql(a, b);
    }
};

pub const MonomorphizationCache = struct {
    allocator: std.mem.Allocator,

    specializations: std.HashMap(MonoKey, []const u8, MonoKeyContext, std.hash_map.default_max_load_percentage),

    functions: std.StringHashMap(*FunctionStatement),
    generated: std.StringHashMap(void),

    pub fn init(allocator: std.mem.Allocator) MonomorphizationCache {
        return MonomorphizationCache{
            .allocator = allocator,
            .functions = std.StringHashMap(*FunctionStatement).init(allocator),
            .specializations = std.HashMap(MonoKey, []const u8, MonoKeyContext, std.hash_map.default_max_load_percentage).init(allocator),
            .generated = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *MonomorphizationCache) void {
        var iterator = self.specializations.iterator();
        while (iterator.next()) |entry| {
            self.allocator.free(entry.key_ptr.types);
            self.allocator.free(entry.value_ptr.*);
        }

        self.specializations.deinit();
        self.functions.deinit();
        self.generated.deinit();
    }

    pub fn register_generic(self: *MonomorphizationCache, name: []const u8, statement: *FunctionStatement) !void {
        try self.functions.put(name, statement);
    }

    pub fn is_generic(self: *MonomorphizationCache, name: []const u8) bool {
        return self.functions.contains(name);
    }

    pub fn get_generic(self: *MonomorphizationCache, name: []const u8) ?*FunctionStatement {
        return self.functions.get(name);
    }

    pub fn mark_generated(self: *MonomorphizationCache, mangled_name: []const u8) !void {
        try self.generated.put(mangled_name, {});
    }

    pub fn get_or_create(
        self: *MonomorphizationCache,
        name: []const u8,
        types: []const Type,
    ) !struct { name: []const u8, is_generated: bool } {
        const key = MonoKey{ .name = name, .types = types };

        if (self.specializations.get(key)) |mangled| {
            return .{ .name = mangled, .is_generated = true };
        }

        const mangled = try mangle(self.allocator, name, types);

        const types_copy = try self.allocator.alloc(Type, types.len);
        @memcpy(types_copy, types);

        const stored_key = MonoKey{ .name = name, .types = types_copy };
        try self.specializations.put(stored_key, mangled);

        return .{ .name = mangled, .is_generated = false };
    }
};

pub fn mangle(allocator: std.mem.Allocator, name: []const u8, types: []const Type) ![]const u8 {
    if (types.len == 0) {
        return try allocator.dupe(u8, name);
    }

    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);

    try buffer.appendSlice(allocator, name);
    try buffer.appendSlice(allocator, "$");

    for (types, 0..) |@"type", index| {
        if (index > 0) try buffer.appendSlice(allocator, "_");
        try buffer.appendSlice(allocator, @"type".to_string());
    }

    return try buffer.toOwnedSlice(allocator);
}
