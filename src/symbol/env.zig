const std = @import("std");

const symbol_zig = @import("symbol.zig");
const scope_zig = @import("scope.zig");
const node_zig = @import("../ast/node.zig");
const type_zig = @import("../types/type.zig");

const FunctionSymbol = symbol_zig.FunctionSymbol;
const TypeSymbol = symbol_zig.TypeSymbol;
const Scope = scope_zig.Scope;
const VariableSymbol = symbol_zig.VariableSymbol;
const StringContext = std.hash_map.StringContext;
const Visibility = node_zig.Visibility;
const Type = type_zig.Type;

pub const Env = struct {
    allocator: std.mem.Allocator,

    current_scope: *Scope,

    pub fn init(allocator: std.mem.Allocator) !Env {
        const scope = try allocator.create(Scope);
        scope.* = try Scope.init(allocator, "global", null);

        scope.type = .Global;

        var env = Env{
            .allocator = allocator,
            .current_scope = scope,
        };

        try env.builtin_types();

        return env;
    }

    pub fn deinit(self: *Env) void {
        self.current_scope.deinit();
        self.allocator.destroy(self.current_scope);
    }

    pub fn set_current_scope(self: *Env, scope: *Scope) void {
        self.current_scope = scope;
    }

    fn builtin_types(self: *Env) !void {
        const pairs = [_]struct { name: []const u8, t: Type }{
            .{ .name = "string", .t = Type{ .String = {} } },
            .{ .name = "bool", .t = Type{ .Boolean = {} } },
            .{ .name = "void", .t = Type{ .Void = {} } },
            .{ .name = "i8", .t = Type{ .Integer = .{ .is_unsigned = false, .size = 8 } } },
            .{ .name = "i16", .t = Type{ .Integer = .{ .is_unsigned = false, .size = 16 } } },
            .{ .name = "i32", .t = Type{ .Integer = .{ .is_unsigned = false, .size = 32 } } },
            .{ .name = "i64", .t = Type{ .Integer = .{ .is_unsigned = false, .size = 64 } } },
            .{ .name = "u8", .t = Type{ .Integer = .{ .is_unsigned = true, .size = 8 } } },
            .{ .name = "u16", .t = Type{ .Integer = .{ .is_unsigned = true, .size = 16 } } },
            .{ .name = "u32", .t = Type{ .Integer = .{ .is_unsigned = true, .size = 32 } } },
            .{ .name = "u64", .t = Type{ .Integer = .{ .is_unsigned = true, .size = 64 } } },
            .{ .name = "f32", .t = Type{ .Float = .{ .size = 32 } } },
            .{ .name = "f64", .t = Type{ .Float = .{ .size = 64 } } },
        };

        for (pairs) |pair| {
            try self.current_scope.add_type(TypeSymbol{
                .name = pair.name,
                .visibility = Visibility.Private,
                .type = pair.t,
            });
        }
    }
};
