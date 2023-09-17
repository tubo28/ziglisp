const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const TokenTag = enum {
    int,
    symbol,
    lParen,
    rParen,
};

const Token = union(TokenTag) {
    int: i64,
    symbol: []const u8,
    lParen,
    rParen,
};

fn tokenize(code: []const u8) ![]const Token {
    var toks = std.ArrayList(Token).init(alloc);

    var i: usize = 0;
    while (i < code.len) : (i += 1) {
        const ascii = std.ascii;
        if (ascii.isWhitespace(code[i])) continue;

        if (code[i] == '(') {
            try toks.append(Token{ .lParen = {} });
            continue;
        }

        if (code[i] == ')') {
            try toks.append(Token{ .rParen = {} });
            continue;
        }

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (ascii.isDigit(code[i])) {
                i += 1;
            }
            const val = try std.fmt.parseInt(i64, code[begin..i], 10);
            try toks.append(Token{ .int = val });
            continue;
        }

        if (ascii.isAlphabetic(code[i])) {
            var begin = i;
            while (ascii.isAlphanumeric(code[i])) {
                i += 1;
            }
            const sym = code[begin..i];
            try toks.append(Token{ .symbol = sym });
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
    const result = try parseSExpr(get);
    print("parse result: {any}\n", .{result.result});
    try expect(true);
}

// Branc of AST.
// () => Cons { .car = null, .cdr = null }
// (foo) => Cons { .car = ConsCell { .atom = foo }, .cdr = null }
// (foo bar) => Cons { .car = ConsCell { .atom = foo }, .cdr = ConsCell {.cons = Cons { .car = ConsCell { .atom = bar }, .cdr = null } } }
const Cons = struct {
    car: ?*ConsCell,
    cdr: ?*ConsCell,
};

// Leaf of AST.
// Token not enclosed in parens.
const AtomTag = enum {
    number,
    symbol,
};
const Atom = union(AtomTag) {
    number: i64,
    symbol: []const u8,
};

fn newAtom(comptime T: type, value: T) !*Atom {
    const ret: *Atom = try alloc.create(Atom);
    switch (T) {
        i64 => ret.* = Atom{ .number = value },
        []const u8 => ret.* = Atom{ .symbol = value },
        else => @panic("currently atom of only i64 or string are allowed"),
    }
    return ret;
}

// Node of tree.
// Branch if cons, leaf if atom.
const ConsCellTag = enum {
    cons,
    atom,
};
const ConsCell = union(ConsCellTag) {
    cons: *Cons,
    atom: *Atom,
};

const ParseResult = struct {
    result: ?*ConsCell,
    rest: []const Token,
};

fn newAtomConsCell(atom: *Atom) !*ConsCell {
    var ret: *ConsCell = try alloc.create(ConsCell);
    ret.* = ConsCell{ .atom = atom };
    return ret;
}

fn newEmptyConsCell() !*ConsCell {
    var ret: *ConsCell = try alloc.create(ConsCell);
    ret.* = ConsCell{ .cons = try alloc.create(Cons) };
    return ret;
}

// <S-expr> ::= <atom> | "(" <S-expr>* ")"
// Returns null for nil
fn parseSExpr(tokens: []const Token) !ParseResult {
    print("parser called: {any}\n", .{tokens});

    if (tokens.len == 0) {
        // Should panic?
        return ParseResult{ .result = null, .rest = tokens };
    }

    const head = tokens[0];
    const tail = tokens[1..];

    switch (head) {
        Token.lParen => {
            assert(tail.len != 0);

            switch (tail[0]) {
                Token.rParen => {
                    var ret: *ConsCell = try alloc.create(ConsCell);
                    return ParseResult{ .result = ret, .rest = tail[1..] };
                },
                else => {
                    var ret = try newEmptyConsCell();
                    const child = try parseSExpr(tail);
                    ret.cons.car = child.result;
                    return ParseResult{ .result = ret, .rest = child.rest };
                },
            }
        },
        Token.int => |int| {
            const atom = try newAtom(i64, int);
            const cell = try newAtomConsCell(atom);
            return ParseResult{ .result = cell, .rest = tokens[1..] };
        },
        Token.symbol => |symbol| {
            const atom = try newAtom([]const u8, symbol);
            const cell = try newAtomConsCell(atom);
            return ParseResult{ .result = cell, .rest = tokens[1..] };
        },
        Token.rParen => @panic("unbalanced parens"),
    }
}

// <atom> ::= <symbol> | <number>
fn parseAtom(token: Token) Atom {
    switch (token) {
        Token.symbol => |symbol| return Atom{ .kind = AtomTag.Symbol, .symbol = symbol },
        Token.int => |int| return Atom{ .kind = AtomTag.Number, .number = int },
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
