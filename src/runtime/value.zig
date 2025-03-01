pub const std = @import("std");

const Type = enum(u4) {
    nil,
    boolean,
    number,
};

const Val = union(Type) {
    nil: void,
    boolean: bool,
    number: i64,
};

const Ctx = enum(u4) {
    std,
    ret,
};

pub const Value = struct {
    ctx: Ctx,
    val: Val,

    pub fn from(t: anytype) Value {
        if (@TypeOf(t) == bool) {
            return Value{
                .ctx = .std,
                .val = Val{ .boolean = t },
            };
        }

        if (@TypeOf(t) == i64) {
            return Value{
                .ctx = .std,
                .val = Val{ .number = t },
            };
        }

        if (@TypeOf(t) == void or t == void) {
            return Value{
                .ctx = .std,
                .val = Val.nil,
            };
        }

        @compileError("invalid type to lift into value " ++ @typeName(t));
    }

    pub fn ret(self: Value) Value {
        return Value{
            .ctx = .ret,
            .val = self.val,
        };
    }

    pub fn is_ret(self: Value) bool {
        return self.ctx == .ret;
    }

    pub fn truthy(self: Value) bool {
        switch (self.val) {
            .nil => return false,
            .boolean => |b| return b,
            .number => |n| return n != 0,
        }
    }

    pub fn repr(self: Value, alloc: std.mem.Allocator) ![]const u8 {
        switch (self.val) {
            .nil => return try std.fmt.allocPrint(alloc, "null", .{}),
            .boolean => |b| return try std.fmt.allocPrint(alloc, "{}", .{b}),
            .number => |n| return try std.fmt.allocPrint(alloc, "{}", .{n}),
        }
    }

    pub fn integer(self: Value) ?i64 {
        switch (self.val) {
            .number => |n| return n,
            else => return null,
        }
    }

    pub fn eql(self: Value, other: Value) bool {
        if (!self.closed(other)) {
            return false;
        }

        switch (self.val) {
            .nil => return true,
            .boolean => |b| return b == other.val.boolean,
            .number => |n| return n == other.val.number,
        }
    }

    pub fn closed(self: Value, other: Value) bool {
        return @as(Type, self.val) == @as(Type, other.val);
    }
};

test "reprs" {
    const Case = struct { value: Value, repr: []const u8 };

    const cases = [_]Case{
        .{ .value = Value.from(void), .repr = "null" },
        .{ .value = Value.from(true), .repr = "true" },
        .{ .value = Value.from(false), .repr = "false" },
        .{ .value = Value.from(@as(i64, 42)), .repr = "42" },
    };

    for (cases) |case| {
        const r = try case.value.repr(std.testing.allocator);
        defer std.testing.allocator.free(r);

        try std.testing.expectEqualSlices(u8, r, case.repr);
    }
}
