pub const std = @import("std");

pub const Token = enum {
    ident,

    // Insignificants
    comment,
    space,

    // keyword / symbols
    let, // let
    assign,
    semicolon,
    comma,
    function, // fn
    cond, // if
    contra, // else
    loop, // loop
    loop_cond, // while
    ret, // return

    // Values
    int,
    string,
    true,
    false,

    // Operators
    bang,
    plus,
    minus,
    star,
    slash,

    // Combo operators
    eq, // ==
    neq, // !=
    ge, // <=
    le, // >=

    // Contexts
    lparen,
    rparen,
    lcurly,
    rcurly,
    lbracket,
    rbracket,
    langle,
    rangle,
};

pub const Error = error{InvalidChar};

pub const Span = struct {
    start: usize,
    end: usize,
};

pub const Lexer = struct {
    content: []const u8,
    name: ?[]const u8,

    pos: usize,
    hare: usize,

    line: usize,

    pub fn init(content: []const u8, name: ?[]const u8) @This() {
        return @This(){
            .content = content,
            .name = name,

            .pos = 0,
            .hare = 0,

            .line = 1,
        };
    }

    pub fn filename(self: *@This()) ?[]const u8 {
        return self.name;
    }

    pub fn lineno(self: *@This()) usize {
        return self.line;
    }

    pub fn resolve(self: *@This(), s: Span) []const u8 {
        return self.content[s.start..s.end];
    }

    pub fn span(self: *@This()) Span {
        return Span{
            .start = self.pos,
            .end = self.hare,
        };
    }

    pub fn slice(self: *@This()) []const u8 {
        return self.content[self.pos..self.hare];
    }

    pub fn sig(self: *@This()) Error!?Token {
        while (try self.next()) |token| {
            switch (token) {
                Token.comment, Token.space => continue,
                else => return token,
            }
        }

        return null;
    }

    pub fn next(self: *@This()) Error!?Token {
        if (self.hare >= self.content.len) {
            return null;
        }

        self.pos = self.hare;

        switch (self.content[self.hare]) {
            '#' => {
                self.chomp_line_comment();
                return Token.comment;
            },

            ' ', '\t', '\n', '\r' => {
                self.chomp_space();
                return Token.space;
            },

            '=' => return self.peek('=', Token.eq, Token.assign),
            '!' => return self.peek('=', Token.neq, Token.bang),

            ';' => return self.chomp(Token.semicolon),
            ',' => return self.chomp(Token.comma),

            '+' => return self.chomp(Token.plus),
            '-' => return self.chomp(Token.plus),

            '/' => return self.chomp(Token.slash),
            '*' => return self.chomp(Token.star),

            '(' => return self.chomp(Token.lparen),
            ')' => return self.chomp(Token.rparen),
            '{' => return self.chomp(Token.lcurly),
            '}' => return self.chomp(Token.rcurly),
            '[' => return self.chomp(Token.lbracket),
            ']' => return self.chomp(Token.rbracket),

            '<' => return self.peek('=', Token.le, Token.langle),
            '>' => return self.peek('=', Token.ge, Token.rangle),

            '0'...'9' => {
                self.chomp_number();
                return Token.int;
            },

            'a'...'z', 'A'...'Z', '_' => {
                return self.handle_alpha();
            },

            else => {
                std.debug.print("char: {s}\n", .{self.content[self.hare..(self.hare + 1)]});
                return Error.InvalidChar;
            },
        }
    }

    fn chomp_line_comment(self: *@This()) void {
        while (self.hare < self.content.len and self.content[self.hare] != '\n') {
            self.hare += 1;
        }

        if (self.hare < self.content.len) {
            self.hare += 1;
        }
    }

    fn chomp_space(self: *@This()) void {
        while (self.hare < self.content.len) {
            switch (self.content[self.hare]) {
                '\n' => {
                    self.hare += 1;
                    self.line += 1;
                },
                ' ', '\t', '\r' => self.hare += 1,
                else => break,
            }
        }
    }

    fn handle_alpha(self: *@This()) Token {
        switch (self.content[self.hare]) {
            'i' => {
                if (self.is_keyword("if")) {
                    return Token.cond;
                }
            },

            'e' => {
                if (self.is_keyword("else")) {
                    return Token.contra;
                }
            },

            'f' => {
                if (self.is_keyword("fn")) {
                    return Token.function;
                }
            },

            'l' => {
                if (self.is_keyword("loop")) {
                    return Token.loop;
                } else if (self.is_keyword("let")) {
                    return Token.let;
                }
            },

            'r' => {
                if (self.is_keyword("return")) {
                    return Token.ret;
                }
            },

            'w' => {
                if (self.is_keyword("while")) {
                    return Token.loop_cond;
                }
            },
            else => {},
        }

        self.eat_ident();
        return Token.ident;
    }

    fn is_keyword(self: *@This(), keyword: []const u8) bool {
        if (self.pos + keyword.len > self.content.len) {
            return false;
        }

        var hare = self.pos + keyword.len;

        if (!std.mem.eql(u8, self.content[self.pos..hare], keyword)) {
            return false;
        }

        if (hare == self.content.len) {
            self.hare = hare;
            return true;
        }

        switch (self.content[hare]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                return false;
            },

            else => {
                self.hare = hare;
                return true;
            },
        }
    }

    fn eat_ident(self: *@This()) void {
        while (self.hare < self.content.len) {
            switch (self.content[self.hare]) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => self.hare += 1,
                else => break,
            }
        }
    }

    fn chomp_number(self: *@This()) void {
        while (self.hare < self.content.len) {
            switch (self.content[self.hare]) {
                '0'...'9' => self.hare += 1,
                else => break,
            }
        }
    }

    fn chomp(self: *@This(), token: Token) Token {
        self.hare += 1;
        return token;
    }

    fn peek(self: *@This(), comptime ch: u8, succ: Token, fail: Token) Token {
        const i = self.hare + 1;
        if (i < self.content.len) {
            if (self.content[i] == ch) {
                self.hare += 2;
                return succ;
            } else {
                self.hare += 1;
                return fail;
            }
        } else {
            self.hare += 1;
            return fail;
        }
    }
};

test "basic lex" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    var lexer = Lexer.init(input, null);

    const Case = struct { token: Token, content: []const u8 };
    const cases = [_]Case{
        .{ .token = Token.let, .content = "let" },
        .{ .token = Token.ident, .content = "five" },
        .{ .token = Token.assign, .content = "=" },
        .{ .token = Token.int, .content = "5" },
        .{ .token = Token.semicolon, .content = ";" },

        .{ .token = Token.let, .content = "let" },
        .{ .token = Token.ident, .content = "ten" },
        .{ .token = Token.assign, .content = "=" },
        .{ .token = Token.int, .content = "10" },
        .{ .token = Token.semicolon, .content = ";" },

        .{ .token = Token.let, .content = "let" },
        .{ .token = Token.ident, .content = "add" },
        .{ .token = Token.assign, .content = "=" },
        .{ .token = Token.function, .content = "fn" },
        .{ .token = Token.lparen, .content = "(" },
        .{ .token = Token.ident, .content = "x" },
        .{ .token = Token.comma, .content = "," },
        .{ .token = Token.ident, .content = "y" },
        .{ .token = Token.rparen, .content = ")" },
        .{ .token = Token.lcurly, .content = "{" },
        .{ .token = Token.ident, .content = "x" },
        .{ .token = Token.plus, .content = "+" },
        .{ .token = Token.ident, .content = "y" },
        .{ .token = Token.semicolon, .content = ";" },
        .{ .token = Token.rcurly, .content = "}" },
        .{ .token = Token.semicolon, .content = ";" },

        .{ .token = Token.let, .content = "let" },
        .{ .token = Token.ident, .content = "result" },
        .{ .token = Token.assign, .content = "=" },
        .{ .token = Token.ident, .content = "add" },
        .{ .token = Token.lparen, .content = "(" },
        .{ .token = Token.ident, .content = "five" },
        .{ .token = Token.comma, .content = "," },
        .{ .token = Token.ident, .content = "ten" },
        .{ .token = Token.rparen, .content = ")" },
        .{ .token = Token.semicolon, .content = ";" },
    };

    for (cases) |case| {
        try std.testing.expectEqual(@as(?Token, case.token), try lexer.sig());
        try std.testing.expectEqualSlices(u8, case.content, lexer.slice());
    }

    try std.testing.expect(null == try lexer.next());

    try std.testing.expectEqual(lexer.lineno(), 8);
}
