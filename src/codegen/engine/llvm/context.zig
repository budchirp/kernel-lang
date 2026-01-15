const std = @import("std");

const c = @import("c.zig").c;

pub const LLVMCodegenContext = struct {
    llvm: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    target_machine: ?c.LLVMTargetMachineRef = null,
    data_layout: ?c.LLVMTargetDataRef = null,

    pub fn init(name: [:0]const u8) !LLVMCodegenContext {
        const llvm = c.LLVMContextCreate();

        const module = c.LLVMModuleCreateWithNameInContext(name, llvm);
        const builder = c.LLVMCreateBuilderInContext(llvm);

        return LLVMCodegenContext{
            .llvm = llvm,
            .module = module,
            .builder = builder,
        };
    }

    pub fn allocate(self: *LLVMCodegenContext) !void {
        _ = c.LLVMInitializeNativeTarget();
        _ = c.LLVMInitializeNativeAsmPrinter();
        _ = c.LLVMInitializeNativeAsmParser();

        const target_triple = c.LLVMGetDefaultTargetTriple();
        defer c.LLVMDisposeMessage(target_triple);

        var target: c.LLVMTargetRef = undefined;
        var error_msg: [*c]u8 = undefined;
        if (c.LLVMGetTargetFromTriple(target_triple, &target, &error_msg) != 0) {
            std.debug.print("Error getting target: {s}\n", .{error_msg});
            return error.LLVMError;
        }

        self.target_machine = c.LLVMCreateTargetMachine(
            target,
            target_triple,
            "generic",
            "",
            c.LLVMCodeGenLevelDefault,
            c.LLVMRelocDefault,
            c.LLVMCodeModelDefault,
        );

        c.LLVMSetTarget(self.module, target_triple);
        const layout = c.LLVMCreateTargetDataLayout(self.target_machine.?);
        c.LLVMSetModuleDataLayout(self.module, layout);

        self.data_layout = layout;
    }

    pub fn emit_to_file(self: *LLVMCodegenContext, path: [:0]const u8) !void {
        var error_msg: [*c]u8 = undefined;
        if (c.LLVMTargetMachineEmitToFile(
            self.target_machine.?,
            self.module,
            @constCast(path.ptr),
            c.LLVMObjectFile,
            &error_msg,
        ) != 0) {
            std.debug.print("Error emitting to file: {s}\n", .{error_msg});
            return error.LLVMError;
        }
    }

    pub fn deallocate(self: *LLVMCodegenContext) void {
        if (self.target_machine) |tm| {
            c.LLVMDisposeTargetMachine(tm);
        }

        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.llvm);
    }
};
