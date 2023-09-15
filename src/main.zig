const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

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
    int: ?i64 = null,
    symbol: ?[]const u8 = null,
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
            try toks.append(Token{ .kind = TokenKind.Int, .int = val });
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
    const expect = std.testing.expect;

    const code = "( foo 42 )";
    const get = try tokenize(code);
    // print("tokenize result: {any}\n", .{get});
    // const want = [_]Token{
    //     Token{ .kind = TokenKind.LParen },
    //     Token{ .kind = TokenKind.Symbol, .symbol = "foo" },
    //     Token{ .kind = TokenKind.Int, .int = 42 },
    //     Token{ .kind = TokenKind.RParen },
    // };

    // try expect(get.len == want.len);
    // for (get, want) |g, w| {
    //     try expect(g.kind == w.kind);
    // }

    print("parse result: {any}\n", .{get});
    print("parse result: {any}\n", .{parseSExpr(get)});
    try expect(true);
}

const ValueKind = enum {
    Int,
    Symbol,
    Cons,
};

const Cons = struct {
    car: ?Value,
    cdr: ?Value,
};

const Value = struct {
    kind: ValueKind,
    int: ?i64,
    symbol: ?[]const u8 = null,
    cons: ?Cons,
};

const Node = union {
    cons: Cons,
    atom: Atom,
};

// <S-expr> ::= <atom> | "(" <S-expr>* ")"
// Returns null for nil
fn parseSExpr(tokens: []const Token) .{ *Node, []const Token } {
    print("{any}", tokens);

    if (tokens.len == 0) {
        return .{ null, tokens };
    }

    if (tokens[0].kind == TokenKind.LParen) {
        const tmpCar = parseSExpr(tokens[1..]);
        const car = tmpCar[0];
        const tmpCdr = parseSExpr(tmpCar[1]);
        const cdr = tmpCdr[0];
        const rest = tmpCdr[1];
        assert(rest.len >= 1 and rest[0].kind == TokenKind.LParen);
        if (rest.len >= 2) {
            print("triling tokens are ignored: {s}", rest[1..]);
        }
        return Node{ .cons = Cons{ .left = car, .right = cdr } };
    }

    return Node{ .atom = parseAtom(tokens[0]) };
}

const AtomKind = enum {
    Number,
    Symbol,
};

const Atom = struct {
    kind: AtomKind,
    symbol: ?[]const u8 = null,
    number: ?i64 = null,
};

// <atom> ::= <symbol> | <number>
fn parseAtom(token: Token) Atom {
    switch (token.kind) {
        TokenKind.Symbol => return Atom{ .kind = AtomKind.Symbol, .symbol = token.symbol },
        TokenKind.Int => return Atom{ .kind = AtomKind.Number, .number = token.int },
        _ => @panic("failed to parse atom"),
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
