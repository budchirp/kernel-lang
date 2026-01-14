const std = @import("std");

const source_zig = @import("../lexer/source.zig");
const lexer_zig = @import("../lexer/lexer.zig");
const parser_zig = @import("../parser/parser.zig");
const checker_zig = @import("../checker/checker.zig");
const codegen_zig = @import("../codegen/codegen.zig");
const engine_zig = @import("../codegen/engine/engine.zig");
const context_zig = @import("context.zig");
const fs_zig = @import("../utils/fs.zig");

const Source = source_zig.Source;
const Lexer = lexer_zig.Lexer;
const Parser = parser_zig.Parser;
const Checker = checker_zig.Checker;
const Codegen = codegen_zig.Codegen;
const CodegenEngine = engine_zig.CodegenEngine;
const CompilerContext = context_zig.CompilerContext;
const FS = fs_zig.FS;

pub const Compiler = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .allocator = allocator,
        };
    }

    pub fn compile_all(self: *Compiler, source_paths: [][]const u8, output_path: []const u8) !void {
        var object_paths = std.ArrayListUnmanaged([:0]const u8){};
        defer {
            for (object_paths.items) |path| self.allocator.free(path);
            object_paths.deinit(self.allocator);
        }

        for (source_paths) |source_path| {
            const object_path = try self.compile_to_object(source_path);
            try object_paths.append(self.allocator, object_path);
        }

        try self.link(object_paths.items, output_path);
    }

    pub fn compile_to_object(self: *Compiler, source_path: []const u8) ![:0]const u8 {
        const content = try FS.read(self.allocator, source_path);
        defer self.allocator.free(content);

        const source = Source.init(source_path, content);
        var lexer = Lexer.init(self.allocator, source);
        const context = CompilerContext.init(source);

        var parser = try Parser.init(self.allocator, &lexer, context);
        defer parser.deinit();

        var program = try parser.parse_program();
        defer program.deinit();

        var checker = try Checker.init(self.allocator, context, &program);
        defer checker.deinit();

        try checker.check();

        const module_name = try self.allocator.dupeZ(u8, std.fs.path.basename(source_path));
        defer self.allocator.free(module_name);

        program.dump(0);

        var engine = try CodegenEngine.init(self.allocator, .llvm, module_name, context, &program);
        defer engine.deinit();

        var codegen = Codegen.init(&engine);
        try codegen.generate();

        const temp_object_path = try std.fmt.allocPrint(self.allocator, "{s}.o", .{source_path});
        defer self.allocator.free(temp_object_path);
        const object_path = try self.allocator.dupeZ(u8, temp_object_path);

        try engine.emit(object_path);
        engine.dump();

        return object_path;
    }

    fn link(self: *Compiler, object_paths: []const [:0]const u8, output_path: []const u8) !void {
        var argv = std.ArrayListUnmanaged([]const u8){};
        defer argv.deinit(self.allocator);

        try argv.append(self.allocator, "cc");
        for (object_paths) |path| {
            try argv.append(self.allocator, path);
        }

        try argv.append(self.allocator, "-o");
        try argv.append(self.allocator, output_path);
        try argv.append(self.allocator, "-lc");

        var child = std.process.Child.init(argv.items, self.allocator);
        _ = try child.spawnAndWait();
    }
};
