const std = @import("std");

const source_zig = @import("../lexer/source.zig");
const lexer_zig = @import("../lexer/lexer.zig");
const parser_zig = @import("../parser/parser.zig");
const program_zig = @import("../ast/program.zig");
const fs_zig = @import("../utils/fs.zig");
const checker_zig = @import("../checker/checker.zig");
const codegen_zig = @import("../codegen/codegen.zig");
const engine_zig = @import("../codegen/engine/engine.zig");
const compiler_zig = @import("../compiler/compiler.zig");

const Compiler = compiler_zig.Compiler;

pub const CLI = struct {
    allocator: std.mem.Allocator,
    args: []const [:0]u8,

    pub fn init(allocator: std.mem.Allocator, args: []const [:0]u8) CLI {
        return CLI{
            .allocator = allocator,
            .args = args,
        };
    }

    pub fn deinit(_: *CLI) void {}

    pub fn run(self: *CLI) !void {
        if (self.args.len < 2) {
            std.debug.print("Usage: {s} <file>... [-o output]\n", .{self.args[0]});
            return error.NoFileProvided;
        }

        var source_files = std.ArrayListUnmanaged([]const u8){};
        defer source_files.deinit(self.allocator);

        var output: []const u8 = "output";
        var i: usize = 1;
        while (i < self.args.len) : (i += 1) {
            if (std.mem.eql(u8, self.args[i], "-o")) {
                if (i + 1 < self.args.len) {
                    output = self.args[i + 1];
                    i += 1;
                } else {
                    return error.NoOutputProvided;
                }
            } else {
                try source_files.append(self.allocator, self.args[i]);
            }
        }

        if (source_files.items.len == 0) {
            return error.NoFileProvided;
        }

        var compiler = Compiler.init(self.allocator);
        try compiler.compile_all(source_files.items, output);

        std.debug.print("Compilation successful! Output: {s}\n", .{output});
    }
};
