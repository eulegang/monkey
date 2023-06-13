const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Lexer = @import("../lex.zig").Lexer;
const expr = @import("./expr.zig");

pub const Stmt = union(enum) {
    let: LetStmt,
    ret: ReturnStmt,

    pub fn parse(parser: *Parser) !Stmt {
        switch (try parser.current()) {
            Lexer.Token.let => return Stmt{ .let = try LetStmt.parse(parser) },
            Lexer.Token.ret => return Stmt{ .ret = try ReturnStmt.parse(parser) },

            else => {
                std.debug.print("current: {}\n", .{try parser.current()});
                return Parser.Error.ExpectedStatement;
            },
        }
    }

    pub fn format(
        self: Stmt,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .let => |l| try writer.print("{}", .{l}),
            .ret => |r| try writer.print("{}", .{r}),
        }
    }
};

pub const LetStmt = struct {
    ident: usize,
    expr: *expr.Expr,

    pub fn parse(parser: *Parser) !LetStmt {
        try parser.next();
        if (try parser.current() != Lexer.Token.ident) {
            return Parser.Error.ExpectedIdent;
        }
        const ident = try parser.symbols.intern(parser.slice());

        try parser.next();
        if (try parser.current() != Lexer.Token.assign) {
            return Parser.Error.ExpectedAssign;
        }

        try parser.next();
        const e = try expr.Expr.parse(parser);
        errdefer parser.free_expr(e);

        if (try parser.current() != Lexer.Token.semicolon) {
            return Parser.Error.ExpectedTerminal;
        }

        try parser.next();

        return LetStmt{
            .ident = ident,
            .expr = e,
        };
    }

    pub fn format(
        self: LetStmt,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(var id[{}] {})", .{ self.ident, self.expr });
    }
};

pub const ReturnStmt = struct {
    expr: *expr.Expr,

    pub fn parse(parser: *Parser) !ReturnStmt {
        try parser.next();
        const e = try expr.Expr.parse(parser);
        errdefer parser.free_expr(e);

        if (try parser.current() != Lexer.Token.semicolon) {
            return Parser.Error.ExpectedTerminal;
        }

        try parser.next();

        return ReturnStmt{ .expr = e };
    }

    pub fn format(
        self: ReturnStmt,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(ret {})", .{self.expr});
    }
};

pub const IfStmt = struct {};
pub const WhileStmt = struct {};
pub const LoopStmt = struct {};
