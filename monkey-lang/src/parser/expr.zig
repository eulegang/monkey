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

    fn down(self: Precedence) Precedence {
        if (self == Precedence.lowest) {
            return Precedence.lowest;
        }

        return @intToEnum(Precedence, @enumToInt(self) - 1);
    }
};

pub const Expr = union(enum) {
    number: Number,
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
                expr.* = Expr{ .ident = try Ident.parse(parser) };
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

            if (@enumToInt(prec) > @enumToInt(level)) {
                break;
            }

            lhs = try Expr.parseInfix(parser, lhs);
        }

        return lhs;
    }
};

pub const Ident = struct {
    ident: usize,

    pub fn parse(parser: *Parser) Parser.Error!Ident {
        const ident = try parser.symbols.intern(parser.slice());
        try parser.next();

        return Ident{ .ident = ident };
    }
};

pub const Number = struct {
    value: u64,

    pub fn parse(parser: *Parser) Parser.Error!Number {
        const v = std.fmt.parseInt(u64, parser.slice(), 10) catch return Parser.Error.InvalidNumber;

        try parser.next();

        return Number{ .value = v };
    }
};

pub const PreOp = enum(u8) {
    Neg,
    Not,
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
                std.debug.print("choked on non-binary op: {}\n", .{t});
                return Parser.Error.NotExpr;
            },
        }
    }
};

pub const Infix = struct {
    op: BinOp,
    lhs: *Expr,
    rhs: *Expr,
};

test "prefix not" {
    const Lexer = @import("../lex.zig").Lexer;
    const Symbols = @import("sym").Symbols;

    var symbols = try Symbols.init(std.testing.allocator);
    defer symbols.deinit();

    const expiremental = try symbols.intern("expiremental");

    var lexer = Lexer.init("!expiremental", null);
    var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);
    var e = try Expr.parse(&parser);
    defer parser.free_expr(e);

    switch (e.*) {
        Expr.prefix => |prefix| {
            try std.testing.expectEqual(prefix.op, PreOp.Not);
            try std.testing.expectEqual(prefix.expr.*, Expr{ .ident = Ident{ .ident = expiremental } });
        },

        else => try std.testing.expect(false),
    }
}

test "prefix neg" {
    const Lexer = @import("../lex.zig").Lexer;
    const Symbols = @import("sym").Symbols;

    var symbols = try Symbols.init(std.testing.allocator);
    defer symbols.deinit();

    var lexer = Lexer.init("-15", null);
    var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);
    const e = try Expr.parse(&parser);
    defer parser.free_expr(e);

    switch (e.*) {
        Expr.prefix => |prefix| {
            try std.testing.expectEqual(prefix.op, PreOp.Neg);
            try std.testing.expectEqual(prefix.expr.*, Expr{ .number = Number{ .value = 15 } });
        },

        else => try std.testing.expect(false),
    }
}

test "infix add" {
    const Lexer = @import("../lex.zig").Lexer;
    const Symbols = @import("sym").Symbols;

    var symbols = try Symbols.init(std.testing.allocator);
    defer symbols.deinit();

    var lexer = Lexer.init("5 + 1", null);
    var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);
    var e = try Expr.parse(&parser);
    defer parser.free_expr(e);

    switch (e.*) {
        Expr.infix => |infix| {
            try std.testing.expectEqual(infix.op, BinOp.Add);
            try std.testing.expectEqual(infix.lhs.*, Expr{ .number = Number{ .value = 5 } });
            try std.testing.expectEqual(infix.rhs.*, Expr{ .number = Number{ .value = 1 } });
        },

        else => {
            std.debug.print("found: {}\n", .{e});
            try std.testing.expect(false);
        },
    }
}

test "infix cmp" {
    const Lexer = @import("../lex.zig").Lexer;
    const Symbols = @import("sym").Symbols;

    var symbols = try Symbols.init(std.testing.allocator);
    defer symbols.deinit();

    var lexer = Lexer.init("3 * 5 == 10 + 5", null);
    var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);
    var e = try Expr.parse(&parser);
    defer parser.free_expr(e);

    switch (e.*) {
        Expr.infix => |infix| {
            try std.testing.expectEqual(infix.op, BinOp.Eq);

            switch (infix.lhs.*) {
                Expr.infix => |l| {
                    try std.testing.expectEqual(l.op, BinOp.Mult);
                    try std.testing.expectEqual(l.lhs.*, Expr{ .number = Number{ .value = 3 } });
                    try std.testing.expectEqual(l.rhs.*, Expr{ .number = Number{ .value = 5 } });
                },

                else => {
                    std.debug.print("lhs found: {}\n", .{infix.lhs});
                    try std.testing.expect(false);
                },
            }

            switch (infix.rhs.*) {
                Expr.infix => |r| {
                    try std.testing.expectEqual(r.op, BinOp.Add);
                    try std.testing.expectEqual(r.lhs.*, Expr{ .number = Number{ .value = 10 } });
                    try std.testing.expectEqual(r.rhs.*, Expr{ .number = Number{ .value = 5 } });
                },

                else => {
                    std.debug.print("rhs found: {}\n", .{infix.rhs});
                    try std.testing.expect(false);
                },
            }
        },

        else => {
            std.debug.print("root found: {}\n", .{e});
            try std.testing.expect(false);
        },
    }
}
