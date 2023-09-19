const std = @import("std");
const dump = std.debug.print;
const assert = std.debug.assert;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const TokenTag = enum {
    int,
    symbol,
    left,
    right,
    quote,
};

const Token = union(TokenTag) {
    int: i64,
    symbol: []const u8,
    left,
    right,
    quote,
};

fn isSymbolChar(c: u8) bool {
    return !std.ascii.isWhitespace(c) and c != ')' and c != '(' and c != '\'';
}

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
            try toks.append(Token{ .left = {} });
            i += 1;
            continue;
        }

        if (code[i] == ')') {
            try toks.append(Token{ .right = {} });
            i += 1;
            continue;
        }

        if (code[i] == '\'') {
            try toks.append(Token{ .quote = {} });
            i += 1;
            continue;
        }

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (i < code.len and ascii.isDigit(code[i])) {
                i += 1;
            }
            const val = try std.fmt.parseInt(i64, code[begin..i], 10);
            try toks.append(Token{ .int = val });
            continue;
        }

        // All other tokens are treated as symbol
        {
            var begin = i;
            while (i < code.len and isSymbolChar(code[i])) {
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

    // TODO: parse dot notation and use it for expected value in tests
    {
        const code = "(+ 1 2)";
        const get = try eval(code);
        try expect(eq(get, try parse("3")));
    }
    {
        const code = "(+ 1 2 (+ 3 4) (+ 5 (+ 6 7)) 8 9 10)";
        const get = try eval(code);
        try expect(eq(get, try parse("55")));
    }
    {
        const code = "'(1 2 3)";
        const get = try eval(code);
        try expect(eq(get, try parse("(1 2 3)")));
    }
    {
        const code = "(length '(1 2 3))";
        const get = try eval(code);
        try expect(eq(get, try parse("3")));
    }
    {
        const code = "(+ (length '(a b c)) (length '(d e)))";
        const get = try eval(code);
        try expect(eq(get, try parse("5")));
    }
    {
        const code = "(print hello)";
        const get = try eval(code);
        try expect(eq(get, try parse("hello")));
    }
    {
        const code = "(progn (print hello) (print world) (+ (length '(a b c)) (length '(d e))))";
        const get = try eval(code);
        try expect(eq(get, try parse("5")));
    }
}

fn eq(a: *const Value, b: *const Value) bool {
    if (a == nil() or b == nil()) return a == b;
    switch (a.*) {
        Value.number => |aa| switch (b.*) {
            Value.number => |bb| return aa == bb,
            else => return false,
        },
        Value.symbol => |aa| switch (b.*) {
            Value.symbol => |bb| return std.mem.eql(u8, aa, bb),
            else => return false,
        },
        Value.cons => |aa| switch (b.*) {
            Value.cons => |bb| return eq(aa.car, bb.car) and eq(aa.cdr, bb.cdr),
            else => return false,
        },
    }
}

fn eval(code: []const u8) !*const Value {
    const tokens = try tokenize(code);
    const sexpr = try parseSExpr(tokens);
    dump("parse result: {s}\n", .{try toStringDot(sexpr.value)});
    const value = evalValue(sexpr.value);
    dump("eval result: {s}\n", .{try toStringDot(value)});
    return value;
}

fn parse(code: []const u8) !*const Value {
    const tokens = try tokenize(code);
    const sexpr = try parseSExpr(tokens);
    dump("parse result: {s}\n", .{try toStringDot(sexpr.value)});
    return sexpr.value;
}

const Cons = struct {
    car: *Value,
    cdr: *Value,
};

fn newCons(car: *Value, cdr: *Value) !*Cons {
    var cons: *Cons = try alloc.create(Cons);
    cons.* = Cons{ .car = car, .cdr = cdr };
    return cons;
}

var _nil: ?*Value = null;

// nil is a ConsCell such that both its car and cdr are itself.
fn nil() *Value {
    if (_nil) |n| {
        return n;
    } else {
        var n: *Value = alloc.create(Value) catch @panic("errro");
        n.* = Value{ .cons = newCons(n, n) catch @panic("message: []const u8") };
        _nil = n;
        return nil();
    }
}

// Node of tree.
// On tree, it is a branch if cons, otherwise leaf
const ValueTag = enum {
    number,
    symbol,
    cons,
};

const Value = union(ValueTag) {
    number: i64,
    symbol: []const u8,
    cons: *Cons,
};

const ParseResult = struct {
    value: *Value,
    rest: []const Token,
};

fn newNilNilConsCell() !*Value {
    var ret: *Value = try alloc.create(Value);
    ret.* = Value{ .cons = try newCons(nil(), nil()) };
    return ret;
}

fn newConsCell(car: *Value, cdr: *Value) !*Value {
    var ret: *Value = try alloc.create(Value);
    ret.* = Value{ .cons = try newCons(car, cdr) };
    return ret;
}

fn newAtom(comptime T: type, value: T) !*Value {
    var ret: *Value = try alloc.create(Value);
    switch (T) {
        i64 => ret.* = Value{ .number = value },
        []const u8 => ret.* = Value{ .symbol = value },
        else => @panic("currently atom of only i64 or string are implemented"),
    }
    return ret;
}

fn toStringDot(cell: *const Value) ![]const u8 {
    var builder = std.ArrayList(u8).init(alloc);
    defer builder.deinit();
    try toStringDotInner(cell, &builder);
    return builder.toOwnedSlice();
}

fn toStringDotInner(cell: *const Value, builder: *std.ArrayList(u8)) !void {
    if (cell == nil()) {
        try builder.appendSlice("nil");
        return;
    }
    switch (cell.*) {
        Value.cons => |cons| {
            try builder.appendSlice("(");
            try toStringDotInner(cons.car, builder);
            try builder.appendSlice(" . ");
            try toStringDotInner(cons.cdr, builder);
            try builder.appendSlice(")");
        },
        Value.number => |num| {
            var buffer: [20]u8 = undefined;
            const str = try std.fmt.bufPrint(buffer[0..], "{}", .{num});
            try builder.appendSlice(str);
        },
        Value.symbol => |sym| try builder.appendSlice(sym),
    }
}

// <sexpr>  ::= <atom>
//            | '(' <sexpr>* ')'
//            | <quote> <sexpr>
fn parseSExpr(tokens: []const Token) anyerror!ParseResult {
    if (tokens.len == 0) {
        // Should panic?
        @panic("no tokens");
        // return ParseResult{ .result = null, .rest = tokens };
    }

    const head = tokens[0];
    const tail = tokens[1..];

    switch (head) {
        Token.left => {
            assert(tail.len != 0);
            var ret = try parseList(tail);
            ret.rest = ret.rest[1..]; // consume ")"
            return ret;
        },
        Token.quote => {
            // <quote> <sexpr> => (quote <sexpr>)
            var listResult = try parseSExpr(tail);
            const quote = try newAtom([]const u8, "quote");
            return ParseResult{ .value = try newConsCell(quote, try newConsCell(listResult.value, nil())), .rest = listResult.rest };
        },
        Token.int => |int| {
            const atom = try newAtom(i64, int);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.symbol => |symbol| {
            const atom = try newAtom([]const u8, symbol);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.right => @panic("unbalanced parens"),
    }
}

// <S-expr>*
fn parseList(tokens: []const Token) anyerror!ParseResult {
    if (tokens.len == 0) {
        return ParseResult{ .value = try newNilNilConsCell(), .rest = tokens };
    }

    switch (tokens[0]) {
        TokenTag.right => return ParseResult{ .value = nil(), .rest = tokens },
        else => {
            // Parse first S-expr
            const car = try parseSExpr(tokens);
            // Parse following S-exprs
            const cdr = try parseList(car.rest);
            // Meld the results of them
            const ret = try newConsCell(car.value, cdr.value);
            return ParseResult{ .value = ret, .rest = cdr.rest };
        },
    }
}

fn _car(cons: *const Value) *const Value {
    switch (cons.*) {
        Value.cons => |c| return c.car,
        else => @panic("car for atom is invalid"),
    }
}

fn _cdr(cons: *const Value) *const Value {
    switch (cons.*) {
        Value.cons => |c| return c.cdr,
        else => @panic("cdr for atom is invalid"),
    }
}

fn _caar(cons: *const Value) *const Value {
    return _car(_car(cons));
}

fn _cadr(cons: *const Value) *const Value {
    return _car(_cdr(cons));
}

fn _cadar(cons: *const Value) *const Value {
    return _car(_cdr(_car(cons)));
}

fn _caddr(cons: *const Value) *const Value {
    return _car(_cdr(_cdr(cons)));
}

fn _caddar(cons: *const Value) *const Value {
    return _car(_cdr(_cdr(_car(cons))));
}

fn _atomp(cons: *const Value) ?*const Value {
    switch (cons.*) {
        Value.cons => return null,
        else => return cons,
    }
}

fn _numberp(atom: *const Value) ?i64 {
    switch (atom.*) {
        Value.number => |num| return num,
        else => return null,
    }
}

fn evalValue(x: *const Value) *const Value {
    dump("+ evalValue: {s}\n", .{toStringDot(x) catch unreachable});

    switch (x.*) {
        Value.symbol, Value.number => return x,
        Value.cons => {
            const car = _car(x);
            switch (car.*) {
                Value.symbol => |sym| {
                    const eql = std.mem.eql;
                    // Built-in functions
                    if (eql(u8, sym, "car"))
                        return evalValue(_cdr(x));
                    if (eql(u8, sym, "cdr"))
                        return evalValue(_cdr(x));
                    if (eql(u8, sym, "print")) {
                        const ret = print(evalValue(_car(_cdr(x))));
                        return ret;
                    }
                    if (eql(u8, sym, "+")) {
                        const ret = add(_cdr(x));
                        return newAtom(i64, ret) catch unreachable;
                    }
                    if (eql(u8, sym, "length")) {
                        const ret = length(evalValue(_car(_cdr(x))));
                        return newAtom(i64, ret) catch unreachable;
                    }
                    // Special forms
                    if (eql(u8, sym, "quote"))
                        return _car(_cdr(x));
                    if (eql(u8, sym, "progn"))
                        return progn(_cdr(x));
                    std.log.err("umimplemented function: {s}\n", .{sym});
                    unreachable;
                },
                Value.number => @panic("number cannot be a function"),
                Value.cons => @panic("cons cannot be a function"),
            }
        },
    }
}

fn progn(x: *const Value) *const Value {
    if (x == nil()) return nil();
    switch (x.*) {
        Value.cons => {
            const val = evalValue(_car(x));
            if (_cdr(x) == nil()) return val;
            return progn(_cdr(x));
        },
        else => @panic("wrong type argument"),
    }
}

fn add(x: *const Value) i64 {
    if (x == nil()) return 0;
    switch (x.*) {
        Value.cons => {
            const lhsValue = evalValue(_car(x));
            const lhs = _numberp(_atomp(lhsValue).?).?;
            return lhs + add(_cdr(x));
        },
        else => @panic("wrong type argument"),
    }
}

fn length(x: *const Value) i64 {
    dump("+ length: {s}\n", .{toStringDot(x) catch unreachable});
    if (x == nil()) return 0;
    switch (x.*) {
        Value.cons => return 1 + length(_cdr(x)),
        else => @panic("cannot apply length for atom"),
    }
}

fn print(x: *const Value) *const Value {
    const str = toStringDot(x) catch unreachable;
    dump("#print: {s}\n", .{str});
    return x;
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
        dump("{s}\n", .{line});
        std.time.sleep(1000_0000);
    }
}
