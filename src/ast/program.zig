const std = @import("std");

const position_zig = @import("../lexer/position.zig");
const node_zig = @import("node.zig");
const type_zig = @import("../types/type.zig");
const env_zig = @import("../symbol/env.zig");

const Position = position_zig.Position;
const Node = node_zig.Node;
const Env = env_zig.Env;

pub const Program = struct {
    position: Position,

    env: *Env,

    statements: std.ArrayListUnmanaged(Node),
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |*statement| {
            statement.deinit();
        }

        self.statements.deinit(self.allocator);

        self.env.deinit();
        self.allocator.destroy(self.env);
    }

    pub fn dump(self: Program, depth: usize) void {
        std.debug.print("Program(statements: [", .{});
        if (self.statements.items.len > 0) {
            std.debug.print("\n", .{});
            for (self.statements.items, 0..) |statement, i| {
                if (i > 0) std.debug.print(",\n", .{});
                type_zig.print_indent(depth + 1);
                statement.dump(depth + 1);
            }
            std.debug.print("\n", .{});
            type_zig.print_indent(depth);
        }
        std.debug.print("])", .{});
    }
};
