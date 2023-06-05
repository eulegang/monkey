const std = @import("std");
const lang = @import("lang");

pub fn main() !void {
    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stdin_file = std.io.getStdIn().reader();
    var br = std.io.bufferedReader(stdin_file);
    const stdin = br.reader();

    try stdout.print("Welcome to the monkey lexer interpreter\n", .{});
    try bw.flush(); // don't forget to flush!

    try stdout.print("\x1b[1m\x1b[32m>>> \x1b[0m", .{});
    try bw.flush();

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
        try bw.flush();
    }
}
