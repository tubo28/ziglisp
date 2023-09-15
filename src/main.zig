const std = @import("std");
const print = std.debug.print;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const TokenKind = enum {
    Int,
    Symbol,
    LParen,
    RParen,
};

const Token = struct {
    kind: TokenKind,
    symbol: ?[]const u8 = null,
    intValue: ?i64 = null,
};

fn tokenize(code: []const u8) ![]const Token {
    var toks = std.ArrayList(Token).init(alloc);

    var i: usize = 0;
    while (i < code.len) : (i += 1) {
        const ascii = std.ascii;
        if (ascii.isWhitespace(code[i])) continue;

        if (code[i] == '(') {
            try toks.append(Token{ .kind = TokenKind.LParen });
            continue;
        }

        if (code[i] == ')') {
            try toks.append(Token{ .kind = TokenKind.RParen });
            continue;
        }

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (ascii.isDigit(code[i])) {
                i += 1;
            }
            const val = try std.fmt.parseInt(i64, code[begin..i], 10);
            try toks.append(Token{ .kind = TokenKind.Int, .intValue = val });
            continue;
        }

        if (ascii.isAlphabetic(code[i])) {
            var begin = i;
            while (ascii.isAlphanumeric(code[i])) {
                i += 1;
            }
            const sym = code[begin..i];
            try toks.append(Token{ .kind = TokenKind.Symbol, .symbol = sym });
            continue;
        }

        @panic("failed to tokenize");
    }
    return toks.items;
}

test "tokenize" {
    const code = "( foo 42 )";
    const get = try tokenize(code);
    print("a: {any}\n", .{get});
    const want = [_]Token{
        Token{ .kind = TokenKind.LParen },
        Token{ .kind = TokenKind.Symbol, .symbol = "foo" },
        Token{ .kind = TokenKind.Int, .intValue = 42 },
        Token{ .kind = TokenKind.RParen },
    };

    const expect = std.testing.expect;
    try expect(get.len == want.len);
    for (get, want) |g, w| {
        try expect(g.kind == w.kind);
    }
}

var lineBuffer: [100]u8 = undefined;

fn readLine(reader: anytype) !?[]const u8 {
    var fbs = std.io.fixedBufferStream(&lineBuffer);
    try reader.streamUntilDelimiter(fbs.writer(), '\n', null);
    return fbs.getWritten();
}

fn rl(reader: anytype) ?[]const u8 {
    return readLine(reader) catch null;
}

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    while (rl(stdin)) |line| {
        print("{s}\n", .{line});
        std.time.sleep(1000_0000);
    }
}
