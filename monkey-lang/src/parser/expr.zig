const std = @import("std");

const Parser = @import("../parser.zig").Parser;
const Token = @import("../lex.zig").Lexer.Token;

const Precedence = enum(u8) {
    lowest = 1, // None
    equals, // ==, !=
    compare, // >, <
    sum, // +
    product, // *
    prefix, // !, -
    call, // fn(x)

    fn prec(token: Token) Precedence {
        switch (token) {
            Token.eq, Token.neq => return Precedence.equals,
            Token.rangle, Token.langle, Token.le, Token.ge => return Precedence.compare,
            Token.plus, Token.minus => return Precedence.sum,
            Token.star, Token.slash => return Precedence.product,

            else => return Precedence.lowest,
        }
    }
};

pub const Expr = union(enum) {
    number: Number,
    boolean: Bool,
    ident: Ident,
    prefix: Prefix,
    infix: Infix,

    pub fn parse(parser: *Parser) Parser.Error!*Expr {
        const e = Expr.parsePrec(parser, Precedence.lowest);

        const token = parser.peek_token() catch return e;

        if (token == Token.semicolon) {
            try parser.next();
        }

        return e;
    }

    fn parsePrefix(parser: *Parser) Parser.Error!*Expr {
        switch (try parser.current()) {
            Token.int => {
                var expr = try parser.alloc.create(Expr);
                expr.* = Expr{ .number = try Number.parse(parser) };
                return expr;
            },

            Token.ident => {
                var expr = try parser.alloc.create(Expr);

                if (Bool.parse(parser)) |b| {
                    expr.* = Expr{ .boolean = b };
                } else {
                    expr.* = Expr{ .ident = try Ident.parse(parser) };
                }
                return expr;
            },

            Token.bang => {
                var expr = try parser.alloc.create(Expr);
                expr.* = Expr{ .prefix = try Prefix.parse(parser) };
                return expr;
            },

            Token.minus => {
                var expr = try parser.alloc.create(Expr);
                expr.* = Expr{ .prefix = try Prefix.parse(parser) };
                return expr;
            },

            Token.lparen => {
                try parser.next();
                const expr = try Expr.parsePrec(parser, Precedence.lowest);

                const token = parser.current() catch return Parser.Error.NotExpr;
                if (token != Token.rparen) {
                    return Parser.Error.NotExpr;
                }

                try parser.next();

                return expr;
            },

            else => {
                return try Expr.parsePrec(parser, Precedence.prefix);
            },
        }
    }

    fn parseInfix(parser: *Parser, lhs: *Expr) Parser.Error!*Expr {
        const token = parser.current() catch return lhs;

        const prec = Precedence.prec(token);

        const op = try BinOp.from(token);

        try parser.next();

        const rhs = try Expr.parsePrec(parser, prec);

        var res = try parser.alloc.create(Expr);
        res.* = Expr{
            .infix = Infix{
                .op = op,
                .lhs = lhs,
                .rhs = rhs,
            },
        };

        return res;
    }

    fn parsePrec(parser: *Parser, prec: Precedence) Parser.Error!*Expr {
        var lhs = try Expr.parsePrefix(parser);

        while (true) {
            var p = parser.current() catch break;

            if (p == Token.semicolon) {
                break;
            }

            const level = Precedence.prec(p);

            if (@enumToInt(prec) >= @enumToInt(level)) {
                break;
            }

            lhs = try Expr.parseInfix(parser, lhs);
        }

        return lhs;
    }

    pub fn format(
        self: Expr,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .ident => |ident| try writer.print("{}", .{ident}),
            .number => |number| try writer.print("{}", .{number}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .prefix => |prefix| try writer.print("{}", .{prefix}),
            .infix => |infix| try writer.print("{}", .{infix}),
        }
    }
};

pub const Ident = struct {
    ident: usize,

    fn parse(parser: *Parser) Parser.Error!Ident {
        const ident = try parser.symbols.intern(parser.slice());
        try parser.next();

        return Ident{ .ident = ident };
    }

    pub fn format(
        self: Ident,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("id[{x}]", .{self.ident});
    }
};

pub const Number = struct {
    value: u64,

    fn parse(parser: *Parser) Parser.Error!Number {
        const v = std.fmt.parseInt(u64, parser.slice(), 10) catch return Parser.Error.InvalidNumber;

        try parser.next();

        return Number{ .value = v };
    }

    pub fn format(
        self: Number,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{}", .{self.value});
    }
};

pub const Bool = struct {
    value: bool,

    fn parse(parser: *Parser) ?Bool {
        const cur = parser.slice();

        if (std.mem.eql(u8, cur, "true")) {
            return Bool{ .value = true };
        }

        if (std.mem.eql(u8, cur, "false")) {
            return Bool{ .value = false };
        }

        return null;
    }

    pub fn format(
        self: Bool,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.value) {
            try writer.print("true", .{});
        } else {
            try writer.print("false", .{});
        }
    }
};

pub const PreOp = enum(u8) {
    Neg,
    Not,

    pub fn format(
        self: PreOp,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Neg => try writer.print("-", .{}),
            .Not => try writer.print("!", .{}),
        }
    }
};

pub const Prefix = struct {
    op: PreOp,
    expr: *Expr,

    fn parse(parser: *Parser) Parser.Error!Prefix {
        const op = switch (try parser.current()) {
            Token.minus => PreOp.Neg,
            Token.bang => PreOp.Not,
            else => unreachable,
        };

        try parser.next();

        const e = try Expr.parsePrec(parser, Precedence.prefix);

        return Prefix{
            .op = op,
            .expr = e,
        };
    }

    pub fn format(
        self: Prefix,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({} {})", .{ self.op, self.expr });
    }
};

pub const BinOp = enum(u8) {
    Eq,
    Neq,

    Lt,
    Le,
    Gt,
    Ge,

    Add,
    Sub,

    Mult,
    Div,

    fn from(t: Token) Parser.Error!BinOp {
        switch (t) {
            Token.eq => return BinOp.Eq,
            Token.neq => return BinOp.Neq,

            Token.langle => return BinOp.Lt,
            Token.le => return BinOp.Le,
            Token.rangle => return BinOp.Gt,
            Token.ge => return BinOp.Ge,

            Token.plus => return BinOp.Add,
            Token.minus => return BinOp.Sub,

            Token.star => return BinOp.Mult,
            Token.slash => return BinOp.Div,

            else => {
                return Parser.Error.NotExpr;
            },
        }
    }

    pub fn format(
        self: BinOp,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Eq => try writer.print("==", .{}),
            .Neq => try writer.print("!=", .{}),

            .Lt => try writer.print("<", .{}),
            .Gt => try writer.print(">", .{}),
            .Le => try writer.print("<=", .{}),
            .Ge => try writer.print(">=", .{}),

            .Add => try writer.print("+", .{}),
            .Sub => try writer.print("-", .{}),
            .Mult => try writer.print("*", .{}),
            .Div => try writer.print("/", .{}),
        }
    }
};

pub const Infix = struct {
    op: BinOp,
    lhs: *Expr,
    rhs: *Expr,

    pub fn format(
        self: Infix,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({} {} {})", .{ self.op, self.lhs, self.rhs });
    }
};

test "exprs reprs" {
    const Lexer = @import("../lex.zig").Lexer;
    const Symbols = @import("sym").Symbols;

    const Case = struct {
        input: []const u8,
        sexp: []const u8,
    };

    const cases = [_]Case{
        .{ .input = "1 + 2", .sexp = "(+ 1 2)" },
        .{ .input = "!expiremental", .sexp = "(! id[0])" },
        .{ .input = "-15", .sexp = "(- 15)" },
        .{ .input = "5 + 1", .sexp = "(+ 5 1)" },
        .{ .input = "3 * 5 == 10 + 5", .sexp = "(== (* 3 5) (+ 10 5))" },
        .{ .input = "1 + 2 + 3", .sexp = "(+ (+ 1 2) 3)" },
        .{ .input = "1 + (2 + 3)", .sexp = "(+ 1 (+ 2 3))" },
        .{ .input = "2 / (5 + 5)", .sexp = "(/ 2 (+ 5 5))" },
        .{ .input = "(5 + 5) * 2", .sexp = "(* (+ 5 5) 2)" },
    };

    for (cases) |case| {
        var symbols = try Symbols.init(std.testing.allocator);
        defer symbols.deinit();

        var lexer = Lexer.init(case.input, null);
        var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);
        var e = try Expr.parse(&parser);
        defer parser.free_expr(e);

        const sexp = try std.fmt.allocPrint(std.testing.allocator, "{}", .{e});
        defer std.testing.allocator.free(sexp);

        try std.testing.expectEqualSlices(u8, case.sexp, sexp);
    }
}
