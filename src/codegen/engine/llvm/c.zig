pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/Linker.h");
    @cInclude("llvm-c/Types.h");
    @cInclude("llvm-c/IRReader.h");
    @cInclude("llvm-c/Support.h");
    @cInclude("llvm-c/Transforms/PassBuilder.h");
    @cInclude("llvm-c/Analysis.h");
});
