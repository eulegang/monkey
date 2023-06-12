const std = @import("std");
const sym = @import("sym");
//const Symbols = @import("sym").Symbols;

const stmt = @import("./parser/stmt.zig");
const expr = @import("./parser/expr.zig");

const Lexer = @import("./lex.zig").Lexer;

pub const Parser = struct {
    pub const Error = error{ ExpectedStatement, InvalidNumber, NotExpr } || Lexer.Error || std.mem.Allocator.Error || sym.Error;

    symbols: *sym.Symbols,
    alloc: std.mem.Allocator,

    lexer: *Lexer,

    cur_slice: []const u8,
    cur: ?Lexer.Token,

    peek_slice: []const u8,
    peek: ?Lexer.Token,

    pub fn init(symbols: *sym.Symbols, alloc: std.mem.Allocator, lexer: *Lexer) Error!@This() {
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
            stmt.Stmt.cond => {},
            stmt.Stmt.loop_cond => {},
            stmt.Stmt.loop => {},
        }
    }

    pub fn free_expr(self: *@This(), e: *expr.Expr) void {
        switch (e.*) {
            expr.Expr.number => {},
            expr.Expr.ident => {},
            expr.Expr.boolean => {},
            expr.Expr.prefix => |prefix| {
                self.free_expr(prefix.expr);
            },

            expr.Expr.infix => |infix| {
                self.free_expr(infix.lhs);
                self.free_expr(infix.rhs);
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

    pub fn current(self: *@This()) !Lexer.Token {
        if (self.cur) |cur| {
            return cur;
        } else {
            return Error.EOF;
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
    };
};

test "parse let" {
    const input =
        \\let five = 5;
        \\let ten = 10;
    ;

    var lexer = Lexer.init(input, null);

    var symbols = try sym.Symbols.init(std.testing.allocator);
    defer symbols.deinit();

    var parser = try Parser.init(&symbols, std.testing.allocator, &lexer);

    var prog = try parser.parse();
    defer parser.free(&prog);

    const five = try symbols.intern("five");
    const ten = try symbols.intern("ten");

    try std.testing.expectEqual(prog.stmts.items[0].let.ident, five);
    try std.testing.expectEqual(prog.stmts.items[0].let.expr.*, expr.Expr{ .number = expr.Number{ .value = 5 } });
    try std.testing.expectEqual(prog.stmts.items[1].let.ident, ten);
    try std.testing.expectEqual(prog.stmts.items[1].let.expr.*, expr.Expr{ .number = expr.Number{ .value = 10 } });
}
