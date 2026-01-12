const std = @import("std");

const env_zig = @import("../symbol/env.zig");
const logger_zig = @import("../logger/logger.zig");
const source_zig = @import("../lexer/source.zig");

const Source = source_zig.Source;
const Logger = logger_zig.Logger;
const Env = env_zig.Env;

pub const CompilerContext = struct {
    source: Source,

    logger: Logger,

    pub fn init(source: Source) CompilerContext {
        return CompilerContext{
            .source = source,
            .logger = Logger.init(source),
        };
    }
};
