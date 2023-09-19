const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const TokenTag = enum {
    int,
    symbol,
    left,
    right,
};

const Token = union(TokenTag) {
    int: i64,
    symbol: []const u8,
    left,
    right,
};

fn isSymbolChar(c: u8) bool {
    return !std.ascii.isWhitespace(c) and c != ')' and c != '(';
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

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (ascii.isDigit(code[i])) {
                i += 1;
            }
            const val = try std.fmt.parseInt(i64, code[begin..i], 10);
            try toks.append(Token{ .int = val });
            continue;
        }

        // All other tokens are treated as symbol
        {
            var begin = i;
            while (isSymbolChar(code[i])) {
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

    const code = "(+ 1 2 (+ 3 4) (+ 5 (+ 6 7)) 8 9 10)";
    const get = try tokenize(code);
    print("tok result: {any}\n", .{get});
    const result = try parseSExpr(get);
    printDot(result.value);
    print("parse result: {any}\n", .{result.value});
    print("eval: {any}\n", .{evalValue(result.value)});
}

fn eval(code: []const u8) *const Value {
    const tokens = try tokenize(code);
    const result = try parseSExpr(tokens);
    const ast = result.value;
    printDot(ast);
    print("eval: {any}\n", .{evalValue(ast)});
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

fn printDot(cell: *const Value) void {
    printDotInner(cell);
    print("\n", .{});
}

fn printDotInner(cell: *const Value) void {
    if (cell == nil()) {
        print("nil", .{});
        return;
    }
    switch (cell.*) {
        Value.cons => |cons| {
            print("(", .{});
            printDotInner(cons.car);
            print(" . ", .{});
            printDotInner(cons.cdr);
            print(")", .{});
        },
        Value.number => |num| print("{}", .{num}),
        Value.symbol => |sym| print("{s}", .{sym}),
    }
}

fn printTree(cell: *Value) void {
    printTreeInner(cell, 0);
}

fn printTreeInner(cell: *Value, depth: usize) void {
    switch (cell.*) {
        Value.cons => |cons| {
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
        Value.atom => |atom| {
            for (0..depth) |_| {
                print(" ", .{});
            }
            print("atom {any}\n", .{atom});
        },
    }
}

// <S-expr> ::= <atom> | "(" <S-expr>* ")"
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
        Token.left => {
            assert(tail.len != 0);
            var ret = try parseList(tail);
            print("{any}\n", .{ret});
            ret.rest = ret.rest[1..]; // consume ")"
            return ret;
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
    return switch (x.*) {
        Value.symbol, Value.number => return x,
        Value.cons => {
            const car = _car(x);
            switch (car.*) {
                Value.symbol => |sym| {
                    if (std.mem.eql(u8, sym, "car")) {
                        return _car(_cdr(x));
                    } else if (std.mem.eql(u8, sym, "cdr")) {
                        return _cdr(_cdr(x));
                    } else if (std.mem.eql(u8, sym, "+")) {
                        return newAtom(i64, add(_cdr(x))) catch @panic("failed to create new atom");
                    } else {
                        std.log.err("umimplemented function: {s}\n", .{sym});
                        @panic("unimpelemented function");
                    }
                },
                Value.number => @panic("number cannot be a function"),
                Value.cons => @panic("cons cannot be a function"),
            }
        },
    };
}

fn add(x: *const Value) i64 {
    if (x == nil()) return 0;
    switch (x.*) {
        Value.cons => {
            print("add {}\n", .{printDot(x)});
            const lhsValue = evalValue(_car(x));
            const lhs = _numberp(_atomp(lhsValue).?).?;
            return lhs + add(_cdr(x));
        },
        else => @panic("cannot apply add for atom"),
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
