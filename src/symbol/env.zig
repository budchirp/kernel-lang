const std = @import("std");

const memory_zig = @import("../utils/memory.zig");
const symbol_zig = @import("symbol.zig");
const scope_zig = @import("scope.zig");
const node_zig = @import("../ast/node.zig");
const type_zig = @import("../types/type.zig");

const FunctionSymbol = symbol_zig.FunctionSymbol;
const TypeSymbol = symbol_zig.TypeSymbol;
const Scope = scope_zig.Scope;
const VariableSymbol = symbol_zig.VariableSymbol;
const StringContext = std.hash_map.StringContext;
const Visibility = symbol_zig.Visibility;
const Type = type_zig.Type;
const memory = memory_zig;

pub const Env = struct {
    allocator: std.mem.Allocator,

    current_scope: *Scope,

    pub fn init(allocator: std.mem.Allocator) !Env {
        const scope = try memory.create(allocator, Scope, try Scope.init(allocator, "global", null));
        scope.type = .Global;

        return Env{
            .allocator = allocator,
            .current_scope = scope,
        };
    }

    pub fn set_current_scope(self: *Env, scope: ?*Scope) void {
        self.current_scope = scope orelse self.current_scope;
    }
};
