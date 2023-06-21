pub const std = @import("std");

pub const Type = enum(u8) {
    nil = 0x00,
    boolean = 0x01,
    number = 0x02,
};

pub const Value = union(Type) {
    boolean: Boolean,
    number: Number,
    nil: Nil,

    pub fn from(t: anytype) Value {
        if (@TypeOf(t) == bool) {
            return Value{ .boolean = Boolean{ .value = t } };
        }

        if (@TypeOf(t) == i64) {
            return Value{ .number = Number{ .value = t } };
        }

        if (@TypeOf(t) == void or t == void) {
            return Value{ .nil = Nil{} };
        }

        @compileError("invalid type to lift into value " ++ @typeName(t));
    }

    pub fn type_tag(self: Value) Type {
        return @as(Type, self);
    }

    pub fn repr(self: Value, alloc: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .boolean => |b| return b.repr(alloc),
            .number => |n| return n.repr(alloc),
            .nil => |n| return n.repr(alloc),
        }
    }

    pub fn truthy(self: Value) bool {
        switch (self) {
            .boolean => |x| return x.truthy(),
            .number => |x| return x.truthy(),
            .nil => |x| return x.truthy(),
        }
    }

    pub fn integer(self: Value) ?i64 {
        switch (self) {
            .number => |x| return x.value,
            else => return null,
        }
    }

    pub fn boolean(self: Value) ?bool {
        switch (self) {
            .boolean => |x| return x.value,
            else => return null,
        }
    }

    pub fn eql(self: Value, other: Value) bool {
        if (self.type_tag() != other.type_tag()) {
            return false;
        }

        switch (self) {
            .boolean => |b| {
                return b.eql(other.boolean);
            },
            .number => |n| {
                return n.eql(other.number);
            },
            .nil => |_| {
                return true;
            },
        }
    }
};

pub const Boolean = struct {
    value: bool,

    fn repr(self: Boolean, alloc: std.mem.Allocator) ![]const u8 {
        if (self.value) {
            return try std.fmt.allocPrint(alloc, "true", .{});
        } else {
            return try std.fmt.allocPrint(alloc, "false", .{});
        }
    }

    fn truthy(self: Boolean) bool {
        return self.value;
    }

    fn eql(self: Boolean, other: Boolean) bool {
        return self.value == other.value;
    }
};

pub const Number = struct {
    value: i64,

    fn repr(self: Number, alloc: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{}", .{self.value});
    }

    fn truthy(self: Number) bool {
        return self.value != 0;
    }

    fn eql(self: Number, other: Number) bool {
        return self.value == other.value;
    }
};

pub const Nil = struct {
    fn repr(_: Nil, alloc: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "null", .{});
    }

    fn truthy(_: Nil) bool {
        return false;
    }
};

test "reprs" {
    const Case = struct { value: Value, repr: []const u8 };

    const cases = [_]Case{
        .{ .value = Value{ .nil = Nil{} }, .repr = "null" },
        .{ .value = Value{ .boolean = Boolean{ .value = true } }, .repr = "true" },
        .{ .value = Value{ .boolean = Boolean{ .value = false } }, .repr = "false" },
        .{ .value = Value{ .number = Number{ .value = 42 } }, .repr = "42" },
    };

    for (cases) |case| {
        const r = try case.value.repr(std.testing.allocator);
        defer std.testing.allocator.free(r);

        try std.testing.expectEqualSlices(u8, r, case.repr);
    }
}
