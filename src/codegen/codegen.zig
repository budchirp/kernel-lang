const std = @import("std");

const engine_zig = @import("engine/engine.zig");
const program_zig = @import("../ast/program.zig");

const CodegenEngine = engine_zig.CodegenEngine;
const Program = program_zig.Program;

pub const Codegen = struct {
    engine: *CodegenEngine,

    pub fn init(engine: *CodegenEngine) Codegen {
        return Codegen{
            .engine = engine,
        };
    }

    pub fn generate(self: *Codegen) !void {
        return self.engine.generate();
    }
};
