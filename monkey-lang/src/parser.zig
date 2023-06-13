const std = @import("std");
const sym = @import("sym");

const stmt = @import("./parser/stmt.zig");
const expr = @import("./parser/expr.zig");

const Lexer = @import("./lex.zig").Lexer;

pub const Parser = struct {
    pub const Error = error{ ExpectedStatement, InvalidNumber, NotExpr, InvalidToken } || Lexer.Error || std.mem.Allocator.Error || sym.Error;

    symbols: *sym.Symbols,
    alloc: std.mem.Allocator,

    lexer: *Lexer,

    cur_slice: []const u8,
    cur: ?Lexer.Token,

    peek_slice: []const u8,
    peek: ?Lexer.Token,

    pub fn init(alloc: std.mem.Allocator, lexer: *Lexer) Error!@This() {
        var symbols = try alloc.create(sym.Symbols);
        symbols.* = try sym.Symbols.init(alloc);
        errdefer alloc.destroy(symbols);

        const cur = try lexer.sig();
        const cur_slice = lexer.slice();
        const p = try lexer.sig();
        const peek_slice = lexer.slice();

        return @This(){
            .symbols = symbols,
            .alloc = alloc,

            .lexer = lexer,
            .cur_slice = cur_slice,
            .cur = cur,
            .peek_slice = peek_slice,
            .peek = p,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.symbols.deinit();
        self.alloc.destroy(self.symbols);
    }

    pub fn free(self: *@This(), prog: *Prog) void {
        for (prog.stmts.items) |item| {
            self.free_stmt(item);
        }

        prog.stmts.deinit();
    }

    pub fn free_stmt(self: *@This(), s: stmt.Stmt) void {
        switch (s) {
            stmt.Stmt.let => |let| self.free_expr(let.expr),
            stmt.Stmt.ret => |ret| self.free_expr(ret.expr),
        }
    }

    pub fn free_expr(self: *@This(), e: *expr.Expr) void {
        switch (e.*) {
            .number => {},
            .ident => {},
            .boolean => {},
            .prefix => |prefix| {
                self.free_expr(prefix.expr);
            },

            .infix => |infix| {
                self.free_expr(infix.lhs);
                self.free_expr(infix.rhs);
            },

            .block => |blk| {
                for (blk.stmts.items) |s| {
                    self.free_stmt(s);
                }

                blk.stmts.deinit();
            },

            .cond => |cond| {
                self.free_expr(cond.cond);

                for (cond.cons.stmts.items) |s| {
                    self.free_stmt(s);
                }

                cond.cons.stmts.deinit();
                self.alloc.destroy(cond.cons);

                if (cond.alt) |alt| {
                    for (alt.stmts.items) |s| {
                        self.free_stmt(s);
                    }

                    alt.stmts.deinit();
                    self.alloc.destroy(alt);
                }
            },

            .fun => |fun| {
                for (fun.body.stmts.items) |s| {
                    self.free_stmt(s);
                }

                fun.body.stmts.deinit();
                self.alloc.destroy(fun.body);
                fun.args.deinit();
            },

            .call => |call| {
                self.free_expr(call.expr);
                for (call.args.items) |sub| {
                    self.free_expr(sub);
                }

                call.args.deinit();
            },
        }

        self.alloc.destroy(e);
    }

    pub fn parse(self: *@This()) !Prog {
        var stmts = std.ArrayList(stmt.Stmt).init(self.alloc);
        while (!self.lexer.eof()) {
            try stmts.append(try stmt.Stmt.parse(self));
        }

        return Prog{ .stmts = stmts };
    }

    pub fn next(self: *@This()) !void {
        self.cur = self.peek;
        self.cur_slice = self.peek_slice;

        self.peek = try self.lexer.sig();
        self.peek_slice = self.lexer.slice();
    }

    pub fn expect(self: *@This(), token: Lexer.Token) !void {
        try self.next();
        if (self.cur != token) {
            return Error.InvalidToken;
        }
    }

    pub fn current(self: *@This()) !Lexer.Token {
        if (self.cur) |cur| {
            return cur;
        } else {
            return Error.EOF;
        }
    }

    pub fn currentIs(self: *@This(), token: Lexer.Token) bool {
        if (self.cur) |cur| {
            return cur == token;
        } else {
            return false;
        }
    }

    pub fn peek_token(self: *@This()) !Lexer.Token {
        if (self.peek) |p| {
            return p;
        } else {
            return Error.EOF;
        }
    }

    pub fn slice(self: *@This()) []const u8 {
        return self.cur_slice;
    }

    pub const Prog = struct {
        stmts: std.ArrayList(stmt.Stmt),

        pub fn format(
            self: Prog,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("(prog", .{});
            for (self.stmts.items) |s| {
                try writer.print(" {}", .{s});
            }

            try writer.print(")", .{});
        }
    };

    pub fn format(
        self: Parser,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("cur: {?}, peek: {?}, slice: {s}", .{ self.cur, self.peek, self.cur_slice });
    }
};

test "parse let" {
    const input =
        \\let five = 5;
        \\let ten = 10;
    ;

    var lexer = Lexer.init(input, null);

    var parser = try Parser.init(std.testing.allocator, &lexer);
    defer parser.deinit();

    var prog = try parser.parse();
    defer parser.free(&prog);

    const five = try parser.symbols.intern("five");
    const ten = try parser.symbols.intern("ten");

    try std.testing.expectEqual(prog.stmts.items[0].let.ident, five);
    try std.testing.expectEqual(prog.stmts.items[0].let.expr.*, expr.Expr{ .number = expr.Number{ .value = 5 } });
    try std.testing.expectEqual(prog.stmts.items[1].let.ident, ten);
    try std.testing.expectEqual(prog.stmts.items[1].let.expr.*, expr.Expr{ .number = expr.Number{ .value = 10 } });
}
