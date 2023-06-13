const std = @import("std");

const Parser = @import("../parser.zig").Parser;
const Token = @import("../lex.zig").Lexer.Token;
const stmt = @import("./stmt.zig");

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
            Token.lparen => return Precedence.call,

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
    block: Block,
    cond: Cond,
    fun: Fun,
    call: FunCall,

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
            .int => {
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .number = try Number.parse(parser) };
                return expr;
            },

            .ident => {
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);

                if (Bool.parse(parser)) |b| {
                    expr.* = Expr{ .boolean = b };
                } else {
                    expr.* = Expr{ .ident = try Ident.parse(parser) };
                }

                return expr;
            },

            .bang => {
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .prefix = try Prefix.parse(parser) };
                return expr;
            },

            .minus => {
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .prefix = try Prefix.parse(parser) };
                return expr;
            },

            .lparen => {
                try parser.next();
                const expr = try Expr.parsePrec(parser, Precedence.lowest);
                errdefer parser.free_expr(expr);

                const token = parser.current() catch return Parser.Error.NotExpr;
                if (token != Token.rparen) {
                    return Parser.Error.NotExpr;
                }

                try parser.next();

                return expr;
            },

            .lcurly => {
                const blk = try Block.parse(parser);
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .block = blk };

                return expr;
            },

            .cond => {
                const cond = try Cond.parse(parser);

                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .cond = cond };
                return expr;
            },

            .function => {
                var expr = try parser.alloc.create(Expr);
                errdefer parser.alloc.destroy(expr);
                expr.* = Expr{ .fun = try Fun.parse(parser) };
                return expr;
            },

            else => {
                return try Expr.parsePrec(parser, Precedence.prefix);
            },
        }
    }

    fn parseInfix(parser: *Parser, lhs: *Expr) Parser.Error!*Expr {
        const token = parser.current() catch return lhs;

        if (token == Token.lparen) {
            var res = try parser.alloc.create(Expr);
            errdefer parser.alloc.destroy(res);
            res.* = Expr{ .call = try FunCall.parse(parser, lhs) };

            return res;
        }

        errdefer parser.free_expr(lhs);

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
            .block => |blk| try writer.print("{}", .{blk}),
            .cond => |cond| try writer.print("{}", .{cond}),
            .fun => |fun| try writer.print("{}", .{fun}),
            .call => |call| try writer.print("{}", .{call}),
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

pub const Block = struct {
    stmts: std.ArrayList(stmt.Stmt),

    fn parse(parser: *Parser) !Block {
        try parser.next();
        var s = try stmt.Stmt.parse(parser);
        var stmts = std.ArrayList(stmt.Stmt).init(parser.alloc);
        errdefer stmts.deinit();

        errdefer for (stmts.items) |item| {
            parser.free_stmt(item);
        };

        try stmts.append(s);

        while (true) {
            const token = parser.current() catch return Parser.Error.NotExpr;

            if (token == .rcurly) {
                break;
            }

            s = try stmt.Stmt.parse(parser);
            try stmts.append(s);
        }

        try parser.next();

        return Block{ .stmts = stmts };
    }

    pub fn format(
        self: Block,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(do", .{});
        for (self.stmts.items) |s| {
            try writer.print(" {}", .{s});
        }

        try writer.print(")", .{});
    }
};

pub const Cond = struct {
    cond: *Expr,
    cons: *Block,
    alt: ?*Block,

    fn parse(parser: *Parser) !Cond {
        try parser.expect(Token.lparen);
        try parser.next();
        const cond = try Expr.parsePrec(parser, Precedence.lowest);
        try parser.expect(Token.rparen);
        try parser.expect(Token.lcurly);

        var cons = try parser.alloc.create(Block);
        errdefer parser.alloc.destroy(cons);
        cons.* = try Block.parse(parser);

        var alt: ?*Block = null;
        if (parser.current()) |cur| {
            if (cur == .contra) {
                try parser.next();

                alt = try parser.alloc.create(Block);
                errdefer parser.alloc.destroy(alt.?);
                alt.?.* = try Block.parse(parser);
            }
        } else |_| {}

        return Cond{ .cond = cond, .cons = cons, .alt = alt };
    }

    pub fn format(
        self: Cond,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(if {} {} {?})", .{ self.cond, self.cons, self.alt });
    }
};

pub const Fun = struct {
    args: std.ArrayList(usize),
    body: *Block,

    fn parse(parser: *Parser) !Fun {
        try parser.expect(.lparen);
        try parser.expect(.ident);

        var args = std.ArrayList(usize).init(parser.alloc);
        errdefer args.deinit();

        while (true) {
            const id = try parser.symbols.intern(parser.slice());
            try args.append(id);

            try parser.next();

            const token = parser.current() catch return Parser.Error.NotExpr;

            if (token == .rparen) {
                try parser.next();
                break;
            }

            if (token != .comma) {
                return Parser.Error.NotExpr;
            }

            try parser.next();
        }

        var body = try parser.alloc.create(Block);
        errdefer parser.alloc.destroy(body);
        body.* = try Block.parse(parser);

        return Fun{
            .args = args,
            .body = body,
        };
    }

    pub fn format(
        self: Fun,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(lambda (", .{});
        if (self.args.items.len > 0) {
            try writer.print("id[{}]", .{self.args.items[0]});

            var i: usize = 1;
            while (i < self.args.items.len) {
                try writer.print(" id[{}]", .{self.args.items[i]});
                i += 1;
            }
            try writer.print(") ", .{});
        }

        try writer.print("{}", .{self.body});

        try writer.print(")", .{});
    }
};

pub const FunCall = struct {
    expr: *Expr,
    args: std.ArrayList(*Expr),

    fn parse(parser: *Parser, expr: *Expr) Parser.Error!FunCall {
        var args = std.ArrayList(*Expr).init(parser.alloc);

        try parser.next();

        if (!parser.currentIs(Token.rparen)) {
            while (true) {
                try args.append(try Expr.parsePrec(parser, Precedence.lowest));

                if (parser.currentIs(Token.rparen)) {
                    break;
                }

                if (!parser.currentIs(Token.comma)) {
                    return Parser.Error.NotExpr;
                }

                try parser.next();
            }
        }

        try parser.next();

        return FunCall{
            .expr = expr,
            .args = args,
        };
    }

    pub fn format(
        self: FunCall,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(apply {} (", .{self.expr});

        if (self.args.items.len > 0) {
            try writer.print("{}", .{self.args.items[0]});

            var i: usize = 1;

            while (i < self.args.items.len) {
                try writer.print(" {}", .{self.args.items[i]});
                i += 1;
            }
        }

        try writer.print("))", .{});
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
        .{ .input = "{let a = 5; return a + 1;}", .sexp = "(do (var id[0] 5) (ret (+ id[0] 1)))" },
        .{ .input = "if (true) { return 1; } else { return 0; }", .sexp = "(if true (do (ret 1)) (do (ret 0)))" },
        .{ .input = "if (true) { return 1; }", .sexp = "(if true (do (ret 1)) null)" },
        .{ .input = "fn (x) { return x + 1; }", .sexp = "(lambda (id[0]) (do (ret (+ id[0] 1))))" },
        .{ .input = "exit()", .sexp = "(apply id[0] ())" },
        .{ .input = "inc(1)", .sexp = "(apply id[0] (1))" },
        .{ .input = "add(1, 2)", .sexp = "(apply id[0] (1 2))" },
        .{ .input = "add(1, x)", .sexp = "(apply id[0] (1 id[1]))" },
        .{ .input = "double(add(x, 1))", .sexp = "(apply id[0] ((apply id[1] (id[2] 1))))" },
        .{ .input = "fn(x, y) { return x + y; }(1, 2)", .sexp = "(apply (lambda (id[0] id[1]) (do (ret (+ id[0] id[1])))) (1 2))" },
    };

    for (cases) |case| {
        var symbols = try Symbols.init(std.testing.allocator);
        defer symbols.deinit();

        var lexer = Lexer.init(case.input, null);
        var parser = try Parser.init(std.testing.allocator, &lexer);
        defer parser.deinit();
        var e = try Expr.parse(&parser);
        defer parser.free_expr(e);

        const sexp = try std.fmt.allocPrint(std.testing.allocator, "{}", .{e});
        defer std.testing.allocator.free(sexp);

        try std.testing.expectEqualSlices(u8, case.sexp, sexp);
    }
}
