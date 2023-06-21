pub const std = @import("std");
pub const value = @import("./value.zig");
pub const expr = @import("../parser/expr.zig");
pub const eexpr = @import("./expr.zig");

pub const Evaluator = struct {
    pub const eval_expr = eexpr.eval_expr;
};

test "basic expr eval" {
    const Lexer = @import("../lex.zig").Lexer;
    const Parser = @import("../parser.zig").Parser;
    const Symbols = @import("sym").Symbols;

    const Case = struct { input: []const u8, repr: []const u8 };

    const cases = [_]Case{
        .{ .input = "6", .repr = "6" },
        .{ .input = "true", .repr = "true" },
        .{ .input = "false", .repr = "false" },

        .{ .input = "!true", .repr = "false" },
        .{ .input = "!false", .repr = "true" },
        .{ .input = "!!true", .repr = "true" },
        .{ .input = "!6", .repr = "false" },
        .{ .input = "!0", .repr = "true" },
        .{ .input = "-5", .repr = "-5" },
        .{ .input = "-42", .repr = "-42" },
        .{ .input = "-false", .repr = "null" },
        .{ .input = "!-false", .repr = "true" },
        .{ .input = "!-true", .repr = "true" },

        .{ .input = "5 == 5", .repr = "true" },
        .{ .input = "6 == 5", .repr = "false" },
        .{ .input = "5 != 5", .repr = "false" },
        .{ .input = "6 != 5", .repr = "true" },

        .{ .input = "6 < 6", .repr = "false" },
        .{ .input = "5 < 6", .repr = "true" },
        .{ .input = "42 < 6", .repr = "false" },
        .{ .input = "5 < true", .repr = "null" },
        .{ .input = "42 <= 6", .repr = "false" },
        .{ .input = "6 <= 6", .repr = "true" },
        .{ .input = "5 <= 6", .repr = "true" },

        .{ .input = "10 + 32", .repr = "42" },
        //.{ .input = "true + false", .repr = "true" },
        .{ .input = "42 + false", .repr = "null" },
        //.{ .input = "false + 42", .repr = "null" },

        .{ .input = "5 - 3", .repr = "2" },

        .{ .input = "5 * 3", .repr = "15" },

        .{ .input = "6 / 3", .repr = "2" },

        .{ .input = "if (true) { 10 } else { 15 }", .repr = "10" },
        .{ .input = "if (false) { 10 } else { return 12; 15 }", .repr = "12" },
    };

    for (cases) |case| {
        var symbols = try Symbols.init(std.testing.allocator);
        defer symbols.deinit();

        var lexer = Lexer.init(case.input, null);
        var parser = try Parser.init(std.testing.allocator, &lexer);
        defer parser.deinit();
        var e = try expr.Expr.parse(&parser);
        defer parser.free_expr(e);

        const val = Evaluator.eval_expr(e);

        const repr = try val.repr(std.testing.allocator);
        defer std.testing.allocator.free(repr);

        //std.debug.print("expr: {}\n", .{e});
        try std.testing.expectEqualSlices(u8, case.repr, repr);
    }
}
