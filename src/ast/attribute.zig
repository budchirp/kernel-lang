const std = @import("std");

pub const PackedAttribute = struct {
    pub fn dump(self: PackedAttribute, depth: usize) void {
        _ = self;
        _ = depth;

        std.debug.print("PackedAttribute()\n", .{});
    }
};

pub const StructAttribute = union(enum) {
    @"packed": PackedAttribute,

    pub fn dump(self: StructAttribute, depth: usize) void {
        switch (self) {
            .@"packed" => |p| {
                p.dump(depth);
            },
        }
    }
};

pub const OperatorAttribute = struct {
    pub fn dump(self: OperatorAttribute, depth: usize) void {
        _ = self;
        _ = depth;

        std.debug.print("OperatorAttribute()\n", .{});
    }
};

pub const FunctionAttribute = union(enum) {
    operator: OperatorAttribute,

    pub fn dump(self: FunctionAttribute, depth: usize) void {
        switch (self) {
            .operator => |o| {
                o.dump(depth);
            },
        }
    }
};

pub const Attribute = union(enum) {
    @"struct": StructAttribute,
    function: FunctionAttribute,

    pub fn dump(self: Attribute, depth: usize) void {
        switch (self) {
            .@"struct" => |s| {
                s.dump(depth);
            },
            .function => |f| {
                f.dump(depth);
            },
        }
    }
};
