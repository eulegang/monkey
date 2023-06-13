const std = @import("std");
const lang = @import("lang");

pub fn main() !void {
    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var args = std.process.args();
    _ = args.next();

    if (args.next()) |ty| {
        if (std.mem.eql(u8, ty, "lex")) {
            try lex(stdout, stdin);
        } else if (std.mem.eql(u8, ty, "ast")) {
            try ast(stdout, stdin);
        } else {
            var stderr = std.io.getStdErr().writer();

            try stderr.print("Invalid mode \"{s}\"\n", .{ty});
            std.process.exit(1);
        }
    } else {
        try lex(stdout, stdin);
    }
}

fn lex(stdout: anytype, stdin: anytype) !void {
    try stdout.print("Welcome to the monkey lexer interpreter\n", .{});

    try stdout.print("\x1b[1m\x1b[32m>>> \x1b[0m", .{});

    var buf: [0x4000]u8 = undefined;

    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try stdout.print("\x1b[34m|- \x1b[33m{s}\x1b[0m\n", .{line});
        var lexer = lang.Lexer.init(line, null);

        while (true) {
            if (lexer.sig()) |mtoken| {
                if (mtoken) |token| {
                    try stdout.print("\x1b[36m <\x1b[0m {} \x1b[35m\"{s}\"\x1b[0m\n", .{ token, lexer.slice() });
                } else {
                    break;
                }
            } else |_| {
                try stdout.print("\x1b[31m^^^\x1b[0mInvalid character: \x1b[31m{s}\x1b[0m\n", .{lexer.current_char()});
                break;
            }
        }

        try stdout.print("\x1b[1m\x1b[32m>>>\x1b[0m ", .{});
    }
}

fn ast(stdout: anytype, stdin: anytype) !void {
    try stdout.print("Welcome to the monkey ast interpreter\n", .{});

    try stdout.print("\x1b[1m\x1b[32m>>> \x1b[0m", .{});

    var buf: [0x4000]u8 = undefined;

    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try stdout.print("\x1b[34m|- \x1b[33m{s}\x1b[0m\n", .{line});

        var alloc = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = alloc.deinit();

        var lexer = lang.Lexer.init(line, null);
        var parser = try lang.Parser.init(alloc.allocator(), &lexer);
        defer parser.deinit();

        var prog = parser.parse() catch |e| {
            try stdout.print("\x1b[31m <ERROR {}\x1b[0m\n", .{e});
            try stdout.print("\x1b[1m\x1b[32m>>>\x1b[0m ", .{});
            continue;
        };
        defer parser.free(&prog);

        try stdout.print("\x1b[36m <\x1b[0m\x1b[35m{s}\x1b[0m\n", .{prog});

        try stdout.print("\x1b[1m\x1b[32m>>>\x1b[0m ", .{});
    }
}
