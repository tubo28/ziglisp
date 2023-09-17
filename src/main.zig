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
    while (i < code.len) {
        const ascii = std.ascii;
        if (ascii.isWhitespace(code[i])) {
            i += 1;
            continue;
        }

        if (code[i] == '(') {
            try toks.append(Token{ .lParen = {} });
            i += 1;
            continue;
        }

        if (code[i] == ')') {
            try toks.append(Token{ .rParen = {} });
            i += 1;
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
    _ = expect;

    // const code = "(0 1 (2 () 3 ()) (4 ((()))))";
    const code = "(car (1 2))";
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

    print("tok result: {any}\n", .{get});
    const result = try parseSExpr(get);
    printDot(result.result);
    print("parse result: {any}\n", .{result.result});
    printTree(result.result);

    print("eval: {any}\n", .{evaluate(result.result)});
}

// Branc of AST.
// () => Cons { .car = null, .cdr = null }
// (foo) => Cons { .car = ConsCell { .atom = foo }, .cdr = null }
// (foo bar) => Cons { .car = ConsCell { .atom = foo }, .cdr = ConsCell {.cons = Cons { .car = ConsCell { .atom = bar }, .cdr = null } } }
const Cons = struct {
    car: *ConsCell,
    cdr: *ConsCell,
};

// fn newCons() !*Cons {
//     var cons: *Cons = try alloc.create(Cons);
//     cons.car = nil;
//     cons.cdr = nil;
//     return cons;
// }

fn newCons(car: *ConsCell, cdr: *ConsCell) !*Cons {
    var cons: *Cons = try alloc.create(Cons);
    cons.* = Cons{ .car = car, .cdr = cdr };
    return cons;
}

var _nil: ?*ConsCell = null;

// nil is a ConsCell such that both its car and cdr are itself.
fn nil() *ConsCell {
    if (_nil) |n| {
        return n;
    } else {
        var n: *ConsCell = alloc.create(ConsCell) catch @panic("errro");
        n.* = ConsCell{ .cons = newCons(n, n) catch @panic("message: []const u8") };
        _nil = n;
        return nil();
    }
}

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
    result: *ConsCell,
    rest: []const Token,
};

fn newAtomConsCell(atom: *Atom) !*ConsCell {
    var ret: *ConsCell = try alloc.create(ConsCell);
    ret.* = ConsCell{ .atom = atom };
    return ret;
}

fn newNilNilConsCell() !*ConsCell {
    var ret: *ConsCell = try alloc.create(ConsCell);
    ret.* = ConsCell{ .cons = try newCons(nil(), nil()) };
    return ret;
}

fn newConsCell(car: *ConsCell, cdr: *ConsCell) !*ConsCell {
    var ret: *ConsCell = try alloc.create(ConsCell);
    ret.* = ConsCell{ .cons = try newCons(car, cdr) };
    return ret;
}

fn printDot(cell: *ConsCell) void {
    printDotInner(cell);
    print("\n", .{});
}

fn printDotInner(cell: *ConsCell) void {
    if (cell == nil()) {
        print("nil", .{});
        return;
    }
    switch (cell.*) {
        ConsCell.cons => |cons| {
            print("(", .{});
            printDotInner(cons.car);
            print(" . ", .{});
            printDotInner(cons.cdr);
            print(")", .{});
        },
        ConsCell.atom => |atom| {
            switch (atom.*) {
                Atom.number => |num| print("{}", .{num}),
                Atom.symbol => |sym| print("{s}", .{sym}),
            }
        },
    }
}

fn printTree(cell: *ConsCell) void {
    printTreeInner(cell, 0);
}

fn printTreeInner(cell: *ConsCell, depth: usize) void {
    switch (cell.*) {
        ConsCell.cons => |cons| {
            if (cons.car == nil() and cons.cdr == nil()) {
                for (0..depth) |_| {
                    print(" ", .{});
                }
                print("nil\n", .{});
            } else {
                if (cons.car != nil()) printTreeInner(cons.car, depth + 1);
                if (cons.cdr != nil()) printTreeInner(cons.cdr, depth + 1);
            }
        },
        ConsCell.atom => |atom| {
            for (0..depth) |_| {
                print(" ", .{});
            }
            print("atom {any}\n", .{atom});
        },
    }
}

// <S-expr> ::= <atom> | "(" <S-expr>* ")"
// Returns null for nil
fn parseSExpr(tokens: []const Token) anyerror!ParseResult {
    print("parser called: {any}\n", .{tokens});

    if (tokens.len == 0) {
        // Should panic?
        @panic("no tokens");
        // return ParseResult{ .result = null, .rest = tokens };
    }

    const head = tokens[0];
    const tail = tokens[1..];

    switch (head) {
        Token.lParen => {
            assert(tail.len != 0);
            var ret = try parseList(tail);
            print("{any}\n", .{ret});
            ret.rest = ret.rest[1..]; // consume ")"
            return ret;
            // switch (tail[0]) {
            //     Token.rParen => {
            //         var ret: *ConsCell = try alloc.create(ConsCell);
            //         return ParseResult{ .result = ret, .rest = tail[1..] };
            //     },
            //     else => {
            //         var ret = try newEmptyConsCell();
            //         const carResult = try parseSExpr(tail);
            //         ret.cons.car = carResult.result;
            //         return ParseResult{ .result = ret, .rest = carResult.rest };
            //     },
            // }
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

// <S-expr>*
fn parseList(tokens: []const Token) anyerror!ParseResult {
    if (tokens.len == 0) {
        return ParseResult{ .result = try newNilNilConsCell(), .rest = tokens };
    }

    switch (tokens[0]) {
        TokenTag.rParen => return ParseResult{ .result = nil(), .rest = tokens },
        else => {
            const headResult: ParseResult = try parseSExpr(tokens);
            var ret = try newNilNilConsCell();
            ret.cons.car = headResult.result;
            const tailResult = try parseList(headResult.rest);
            ret.cons.cdr = tailResult.result;
            return ParseResult{ .result = ret, .rest = tailResult.rest };
        },
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

fn _car(cons: *ConsCell) *ConsCell {
    switch (cons.*) {
        ConsCell.cons => |c| return c.car,
        ConsCell.atom => @panic("car for atom is invalid"),
    }
}

fn _cdr(cons: *ConsCell) *ConsCell {
    switch (cons.*) {
        ConsCell.cons => |c| return c.cdr,
        ConsCell.atom => @panic("cdr for atom is invalid"),
    }
}

fn _caar(cons: *ConsCell) *ConsCell {
    return _car(_car(cons));
}

fn _cadr(cons: *ConsCell) *ConsCell {
    return _car(_cdr(cons));
}

fn _cadar(cons: *ConsCell) *ConsCell {
    return _car(_cdr(_car(cons)));
}

fn _caddr(cons: *ConsCell) *ConsCell {
    return _car(_cdr(_cdr(cons)));
}

fn _caddar(cons: *ConsCell) *ConsCell {
    return _car(_cdr(_cdr(_car(cons))));
}

fn _atom(cons: *ConsCell) bool {
    return switch (cons) {
        ConsCellTag.atom => true,
        else => false,
    };
}

const ValueTag = enum {
    number,
    symbol,
};

fn evaluate(consCell: *ConsCell) *ConsCell {
    return switch (consCell.*) {
        ConsCell.atom => return consCell,
        ConsCell.cons => |cons| {
            _ = cons;
            const car = _car(consCell);
            switch (car.*) {
                ConsCell.atom => |atom| switch (atom.*) {
                    Atom.symbol => |sym| {
                        if (std.mem.eql(u8, sym, "car")) {
                            return _car(_cdr(consCell));
                        } else {
                            @panic("function except car is unimplemented");
                        }
                    },
                    Atom.number => @panic("number cannot be a function"),
                },
                ConsCell.cons => @panic("cons cannot be a function"),
            }
        },
    };
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
