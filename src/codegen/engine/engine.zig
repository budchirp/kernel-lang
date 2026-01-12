const std = @import("std");

const llvm_zig = @import("llvm/llvm.zig");
const program_zig = @import("../../ast/program.zig");
const context_zig = @import("../../compiler/context.zig");

const Program = program_zig.Program;
const LLVMCodegen = llvm_zig.LLVMCodegen;
const CompilerContext = context_zig.CompilerContext;

pub const CodegenEngineType = enum {
    llvm,
};

pub const CodegenEngine = union(CodegenEngineType) {
    llvm: LLVMCodegen,

    pub fn init(allocator: std.mem.Allocator, engine_type: CodegenEngineType, name: [:0]const u8, context: CompilerContext, program: *Program) !CodegenEngine {
        return switch (engine_type) {
            .llvm => {
                var codegen = try LLVMCodegen.init(allocator, name, context, program);
                try codegen.allocate();

                return CodegenEngine{
                    .llvm = codegen,
                };
            },
        };
    }

    pub fn deinit(self: *CodegenEngine) void {
        return switch (self.*) {
            .llvm => |*llvm| {
                llvm.deallocate();
                llvm.deinit();
            },
        };
    }

    pub fn generate(self: *CodegenEngine) !void {
        return switch (self.*) {
            .llvm => |*llvm| try llvm.generate(),
        };
    }

    pub fn dump(self: *CodegenEngine) void {
        return switch (self.*) {
            .llvm => |*llvm| llvm.dump(),
        };
    }

    pub fn emit(self: *CodegenEngine, path: [:0]const u8) !void {
        return switch (self.*) {
            .llvm => |*llvm| try llvm.emit(path),
        };
    }
};
