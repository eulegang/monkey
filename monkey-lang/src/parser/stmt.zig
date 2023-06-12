const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Lexer = @import("../lex.zig").Lexer;
const expr = @import("./expr.zig");

pub const Stmt = union(enum) {
    let: LetStmt,
    ret: ReturnStmt,
    cond: IfStmt,
    loop_cond: WhileStmt,
    loop: LoopStmt,

    pub fn parse(parser: *Parser) !Stmt {
        switch (try parser.current()) {
            Lexer.Token.let => return Stmt{ .let = try LetStmt.parse(parser) },
            Lexer.Token.ret => return Stmt{ .ret = try ReturnStmt.parse(parser) },

            else => {
                std.debug.print("current: {}", .{try parser.current()});
                return Parser.Error.ExpectedStatement;
            },
        }
    }
};

pub const LetStmt = struct {
    ident: usize,
    expr: *expr.Expr,

    pub fn parse(parser: *Parser) !LetStmt {
        try parser.next();
        if (try parser.current() != Lexer.Token.ident) {
            @import("std").debug.print("current: {}", .{try parser.current()});
            return Parser.Error.ExpectedIdent;
        }
        const ident = try parser.symbols.intern(parser.slice());

        try parser.next();
        if (try parser.current() != Lexer.Token.assign) {
            return Parser.Error.ExpectedAssign;
        }

        try parser.next();
        const e = try expr.Expr.parse(parser);

        if (try parser.current() != Lexer.Token.semicolon) {
            std.debug.print("current: {}\n", .{try parser.current()});
            return Parser.Error.ExpectedTerminal;
        }

        try parser.next();

        return LetStmt{
            .ident = ident,
            .expr = e,
        };
    }
};

pub const ReturnStmt = struct {
    expr: *expr.Expr,

    pub fn parse(parser: *Parser) !ReturnStmt {
        const e = try expr.Expr.parse(parser);

        try parser.next();
        if (try parser.current() != Lexer.Token.semicolon) {
            return Parser.Error.ExpectedTerminal;
        }

        return ReturnStmt{ .expr = e };
    }
};

pub const IfStmt = struct {};
pub const WhileStmt = struct {};
pub const LoopStmt = struct {};
