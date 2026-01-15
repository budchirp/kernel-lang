const std = @import("std");

const memory_zig = @import("../utils/memory.zig");
const symbol_zig = @import("symbol.zig");
const env_zig = @import("env.zig");
const type_zig = @import("../types/type.zig");

const Type = type_zig.Type;
const Env = env_zig.Env;
const TypeSymbol = symbol_zig.TypeSymbol;
const FunctionSymbol = symbol_zig.FunctionSymbol;
const Visibility = symbol_zig.Visibility;
const memory = memory_zig;

pub const builtin_functions = [_][]const u8{"sizeof"};

pub const Builtin = struct {
    allocator: std.mem.Allocator,

    env: *Env,

    pub fn init(allocator: std.mem.Allocator, env: *Env) !Builtin {
        return Builtin{
            .allocator = allocator,
            .env = env,
        };
    }

    pub fn define(self: *Builtin) !void {
        try self.define_types();
        try self.define_functions();
    }

    fn define_types(self: *Builtin) !void {
        const pairs = [_]struct { name: []const u8, type: Type }{
            .{ .name = "string", .type = Type{ .String = {} } },
            .{ .name = "boolean", .type = Type{ .Boolean = {} } },
            .{ .name = "void", .type = Type{ .Void = {} } },
            .{ .name = "i8", .type = Type{ .Integer = .{ .is_unsigned = false, .size = 8 } } },
            .{ .name = "i16", .type = Type{ .Integer = .{ .is_unsigned = false, .size = 16 } } },
            .{ .name = "i32", .type = Type{ .Integer = .{ .is_unsigned = false, .size = 32 } } },
            .{ .name = "i64", .type = Type{ .Integer = .{ .is_unsigned = false, .size = 64 } } },
            .{ .name = "u8", .type = Type{ .Integer = .{ .is_unsigned = true, .size = 8 } } },
            .{ .name = "u16", .type = Type{ .Integer = .{ .is_unsigned = true, .size = 16 } } },
            .{ .name = "u32", .type = Type{ .Integer = .{ .is_unsigned = true, .size = 32 } } },
            .{ .name = "u64", .type = Type{ .Integer = .{ .is_unsigned = true, .size = 64 } } },
            .{ .name = "f32", .type = Type{ .Float = .{ .size = 32 } } },
            .{ .name = "f64", .type = Type{ .Float = .{ .size = 64 } } },
        };

        for (pairs) |pair| {
            try self.env.current_scope.add_type(TypeSymbol{
                .name = pair.name,
                .visibility = Visibility.Private,
                .type = pair.type,
            });
        }
    }

    fn define_functions(self: *Builtin) !void {
        const pairs = [_]struct { name: []const u8, type: type_zig.FunctionType }{
            .{ .name = "sizeof", .type = try self.sizeof() },
        };

        for (pairs) |pair| {
            try self.env.current_scope.add_function(FunctionSymbol{
                .name = pair.name,
                .visibility = Visibility.Private,
                .type = pair.type,
            });
        }
    }

    fn sizeof(self: *Builtin) !type_zig.FunctionType {
        const return_type = try memory.create(self.allocator, type_zig.Type, type_zig.Type{ .Integer = .{ .is_unsigned = true, .size = 64 } });

        var parameters = std.ArrayList(type_zig.FunctionParameterType){};
        try parameters.append(self.allocator, type_zig.FunctionParameterType{
            .name = "type",
            .type = try memory.create(self.allocator, type_zig.Type, type_zig.Type{ .Any = {} }),
        });

        return type_zig.FunctionType{
            .return_type = return_type,
            .parameters = try parameters.toOwnedSlice(self.allocator),
            .generics = &[_]type_zig.GenericParameterType{},
            .is_variadic = false,
            .is_operator = false,
        };
    }
};
