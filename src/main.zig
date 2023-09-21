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
    nil,
};

const Token = union(TokenTag) {
    int: i64,
    symbol: []const u8,
    left, // (
    right, // )
    quote, // '
    nil, // nil
};

fn isSymbolChar(c: u8) bool {
    return !std.ascii.isWhitespace(c) and c != ')' and c != '(' and c != '\'';
}

fn tokenize(code: []const u8) []const Token {
    var toks = std.ArrayList(Token).init(alloc); // defer deinit?

    var i: usize = 0;
    while (i < code.len) {
        const ascii = std.ascii;
        if (ascii.isWhitespace(code[i])) {
            i += 1;
            continue;
        }

        if (code[i] == '(') {
            toks.append(Token{ .left = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (code[i] == ')') {
            toks.append(Token{ .right = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (code[i] == '\'') {
            toks.append(Token{ .quote = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (i < code.len and ascii.isDigit(code[i])) {
                i += 1;
            }
            const val = std.fmt.parseInt(i64, code[begin..i], 10) catch unreachable;
            toks.append(Token{ .int = val }) catch unreachable;
            continue;
        }

        // All other chars are parts of as symbol
        {
            var begin = i;
            while (i < code.len and isSymbolChar(code[i])) {
                i += 1;
            }
            const sym = code[begin..i];
            // special symbol
            if (std.mem.eql(u8, sym, "nil")) {
                toks.append(Token{ .nil = {} }) catch unreachable;
                continue;
            }
            toks.append(Token{ .symbol = sym }) catch unreachable;
            continue;
        }

        @panic("failed to tokenize");
    }
    return toks.items;
}

fn eval(code: []const u8) *const Value {
    const tokens = tokenize(code);
    const sexpr = parseSExpr(tokens);
    dump("parse result: {s}\n", .{toStringDot(sexpr.value)});
    var env = Map.init(alloc);
    const value = evalValue(sexpr.value, &env);
    dump("eval result: {s}\n", .{toStringDot(value)});
    return value;
}

fn parse(code: []const u8) *const Value {
    const tokens = tokenize(code);
    const sexpr = parseSExpr(tokens);
    dump("parse result: {s}\n", .{toStringDot(sexpr.value)});
    return sexpr.value;
}

const Cons = struct {
    car: *const Value,
    cdr: *const Value,
};

fn newCons(car: *const Value, cdr: *const Value) *Cons {
    var cons: *Cons = alloc.create(Cons) catch unreachable;
    cons.* = Cons{ .car = car, .cdr = cdr };
    return cons;
}

var _nil: ?*Value = null;

// nil is a ConsCell such that both its car and cdr are itself.
fn nil() *const Value {
    if (_nil) |n| {
        return n;
    } else {
        var n: *Value = alloc.create(Value) catch @panic("errro");
        n.* = Value{ .cons = newCons(n, n) };
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
    function,
};

const Value = union(ValueTag) {
    number: i64,
    symbol: []const u8,
    cons: *Cons,
    function: *Function,
};

const Function = struct {
    name: []const u8,
    params: [][]const u8,
    body: *const Value,
    env: *const Map, // captured env. scope?
};

fn newFunc(name: []const u8, params: [][]const u8, body: *const Value, env: *const Map) *Function {
    var ret: *Function = alloc.create(Function) catch unreachable;
    ret.* = Function{
        .name = name,
        .params = params,
        .body = body,
        .env = env,
    };
    return ret;
}

const ParseResult = struct {
    value: *const Value,
    rest: []const Token,
};

fn newNilNilConsCell() *Value { // need this?
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .cons = newCons(nil(), nil()) };
    return ret;
}

fn newConsValue(car: *const Value, cdr: *const Value) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .cons = newCons(car, cdr) };
    return ret;
}

fn newAtomValue(comptime T: type, value: T) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    switch (T) {
        i64 => ret.* = Value{ .number = value },
        []const u8 => ret.* = Value{ .symbol = value },
        else => @panic("currently atom of only i64 or string are implemented"),
    }
    return ret;
}

fn newFuncValue(name: []const u8, params: [][]const u8, body: *const Value, env: *const Map) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .function = newFunc(name, params, body, env) };
    return ret;
}

// fn newFunction(name: []const Value)
fn toStringDot(cell: *const Value) []const u8 {
    var builder = std.ArrayList(u8).init(alloc);
    defer builder.deinit();
    toStringDotInner(cell, &builder);
    return builder.toOwnedSlice() catch unreachable;
}

fn toStringDotInner(cell: *const Value, builder: *std.ArrayList(u8)) void {
    if (cell == nil()) {
        builder.appendSlice("nil") catch unreachable;
        return;
    }
    switch (cell.*) {
        Value.cons => |cons| {
            builder.appendSlice("(") catch unreachable;
            toStringDotInner(cons.car, builder);
            builder.appendSlice(" . ") catch unreachable;
            toStringDotInner(cons.cdr, builder);
            builder.appendSlice(")") catch unreachable;
        },
        Value.number => |num| {
            var buffer: [30]u8 = undefined;
            const str = std.fmt.bufPrint(buffer[0..], "{}", .{num}) catch @panic("too large integer");
            builder.appendSlice(str) catch unreachable;
        },
        Value.symbol => |sym| builder.appendSlice(sym) catch unreachable,
        Value.function => |func| {
            builder.appendSlice("<function:") catch unreachable;
            builder.appendSlice(func.name) catch unreachable;
            builder.appendSlice(">") catch unreachable;
        },
    }
}

// <sexpr>  ::= <atom>
//            | '(' <sexpr>* ')'
//            | <quote> <sexpr>
fn parseSExpr(tokens: []const Token) ParseResult {
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
            var ret = parseList(tail);
            ret.rest = ret.rest[1..]; // consume ")"
            return ret;
        },
        Token.quote => {
            // <quote> <sexpr> => (quote <sexpr>)
            var listResult = parseSExpr(tail);
            const quote = newAtomValue([]const u8, "quote");
            return ParseResult{ .value = newConsValue(quote, newConsValue(listResult.value, nil())), .rest = listResult.rest };
        },
        Token.int => |int| {
            const atom = newAtomValue(i64, int);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.symbol => |symbol| {
            const atom = newAtomValue([]const u8, symbol);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.right => @panic("unbalanced parens"),
        Token.nil => return ParseResult{ .value = nil(), .rest = tokens[1..] },
    }
}

// <S-expr>*
fn parseList(tokens: []const Token) ParseResult {
    if (tokens.len == 0) {
        return ParseResult{ .value = newNilNilConsCell(), .rest = tokens };
    }

    switch (tokens[0]) {
        TokenTag.right => return ParseResult{ .value = nil(), .rest = tokens },
        else => {
            // Parse first S-expr
            const car = parseSExpr(tokens);
            // Parse following S-exprs
            const cdr = parseList(car.rest);
            // Meld the results of them
            const ret = newConsValue(car.value, cdr.value);
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

fn _symbolp(atom: *const Value) ?[]const u8 {
    switch (atom.*) {
        Value.symbol => |sym| return sym,
        else => return null,
    }
}

const Map = std.StringHashMap(*const Value);

fn evalValue(x: *const Value, env: *Map) *const Value {
    // dump("+ evalValue: {s}\n", .{toStringDot(x) catch unreachable});
    switch (x.*) {
        Value.number, Value.function => return x,
        Value.symbol => |sym| {
            if (env.get(sym)) |ent| return ent;
            return x;
        },
        Value.cons => {
            const car = _car(x);
            switch (car.*) {
                Value.number => @panic("number cannot be a function"),
                Value.cons => @panic("cons cell cannot be a function"),
                Value.function => @panic("unimplemented"),
                Value.symbol => |sym| {
                    const eql = std.mem.eql;
                    // Built-in functions
                    if (eql(u8, sym, "car")) {
                        const arg = evalValue(_car(_cdr(x)), env);
                        return _car(arg);
                    }
                    if (eql(u8, sym, "cdr")) {
                        const arg = evalValue(_car(_cdr(x)), env);
                        return _cdr(arg);
                    }
                    if (eql(u8, sym, "cons")) {
                        const argCar = evalValue(_car(_cdr(x)), env);
                        const argCdr = evalValue(_car(_cdr(_cdr(x))), env);
                        return newConsValue(argCar, argCdr);
                    }
                    if (eql(u8, sym, "print")) {
                        const arg = evalValue(_car(_cdr(x)), env);
                        return print(arg);
                    }
                    if (eql(u8, sym, "+")) {
                        const ret = add(_cdr(x), env);
                        return newAtomValue(i64, ret);
                    }
                    if (eql(u8, sym, "length")) {
                        const arg = evalValue(_car(_cdr(x)), env);
                        const ret = length(arg);
                        return newAtomValue(i64, ret);
                    }
                    // Special forms
                    if (eql(u8, sym, "quote"))
                        return _car(_cdr(x));
                    if (eql(u8, sym, "progn"))
                        // TODO: replace progn from special form to lambda and macro
                        return progn(_cdr(x), env);
                    if (eql(u8, sym, "setq"))
                        return setq(_cdr(x), env);
                    if (eql(u8, sym, "defun")) {
                        const name = _car(_cdr(x));
                        const args = _car(_cdr(_cdr(x)));
                        const body = _car(_cdr(_cdr(_cdr((x)))));
                        return defun(name, args, body, env);
                    }

                    std.log.err("umimplemented function: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
}

// Convert list like (foo bar buz) to slice
fn toSlice(head: *const Value) []*const Value {
    var ret = std.ArrayList(*const Value).init(alloc); // defer deinit?
    var h = head;
    while (h != nil()) {
        ret.append(_car(h)) catch @panic("cannot append");
        h = _cdr(h);
    }
    return ret.toOwnedSlice() catch unreachable;
}

// scope is lexical i.e. 'env' is the snapshot of parse's env
fn defun(name: *const Value, params: *const Value, body: *const Value, env: *const Map) *const Value {
    var symbols = std.ArrayList([]const u8).init(alloc);
    var paramsSlice = toSlice(params);
    for (paramsSlice) |a| symbols.append(_symbolp(a).?) catch unreachable;
    return newFuncValue(
        _symbolp(name).?,
        symbols.toOwnedSlice() catch unreachable,
        body,
        env,
    );
}

fn setq(x: *const Value, env: *Map) *const Value {
    if (x == nil()) return nil();
    const sym = _symbolp(_car(x)).?;
    const val = evalValue(_car(_cdr(x)), env);
    env.put(sym, val) catch unreachable;
    return val;
}

fn progn(x: *const Value, env: *Map) *const Value {
    const slice = toSlice(x);
    var ret = nil();
    for (slice) |a| ret = evalValue(a, env);
    return ret; // the last is returned
}

fn add(x: *const Value, env: *Map) i64 {
    const slice = toSlice(x);
    var ret: i64 = 0;
    for (slice) |a| ret += _numberp(evalValue(a, env)).?;
    return ret;
}

fn length(x: *const Value) i64 {
    const slice = toSlice(x);
    return @intCast(slice.len);
}

fn print(x: *const Value) *const Value {
    const str = toStringDot(x);
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

test "tokenize" {
    const TestCase = struct {
        code: []const u8,
        want: *const Value,
    };

    const cases = [_]TestCase{
        TestCase{
            .code = "(+ 1 2)",
            .want = parse("3"),
        },
        TestCase{
            .code = "(+ 1 2 (+ 3 4) (+ 5 (+ 6 7)) 8 9 10)",
            .want = parse("55"),
        },
        TestCase{
            .code = "'(1 2 3)",
            .want = parse("(1 2 3)"),
        },
        TestCase{
            .code = "(length '(1 2 3))",
            .want = parse("3"),
        },
        TestCase{
            .code = "(+ (length '(a b c)) (length '(d e)))",
            .want = parse("5"),
        },
        TestCase{
            .code = "(print hello)",
            .want = parse("hello"),
        },
        TestCase{
            .code = "(progn (print hello) (print world) (+ (length '(a b c)) (length '(d e))))",
            .want = parse("5"),
        },
        TestCase{
            .code = "(setq menu '(tea coffee milk))",
            .want = parse("(tea coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq a 1) (setq b 2) (+ a b 3))",
            .want = parse("6"),
        },
        TestCase{
            .code = "(progn (setq p '(3 1 4 1 5)) (print (length p)))",
            .want = parse("5"),
        },
        TestCase{
            .code = "(car '(a b c))",
            .want = parse("a"),
        },
        TestCase{
            .code = "(car '((a b) (c d)))",
            .want = parse("(a b)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (car menu))",
            .want = parse("tea"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cdr menu))",
            .want = parse("(coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cdr (cdr menu)))",
            .want = parse("(milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cdr (cdr (cdr menu))))",
            .want = parse("nil"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (car (cdr menu)))",
            .want = parse("coffee"),
        },
        TestCase{
            .code = "(cons '(a b) '(c d))",
            .want = parse("((a b) c d)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons 'cocoa menu))",
            .want = parse("(cocoa tea coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons 'cocoa (cdr menu)))",
            .want = parse("(cocoa coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (car (cdr (cdr menu))))",
            .want = parse("milk"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons 'juice (cdr menu))))",
            .want = parse("(juice coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons (car menu) (cons 'juice (cdr menu))))",
            .want = parse("(tea juice coffee milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons (car menu) (cdr (cdr menu))))",
            .want = parse("(tea milk)"),
        },
        TestCase{
            .code = "(progn (setq menu '(tea coffee milk)) (cons (car (cdr menu)) (cons (car menu) (cdr (cdr menu)))))",
            .want = parse("(coffee tea milk)"),
        },
        TestCase{
            .code = "(progn (defun double (x) (+ x x)) (double 1))",
            .want = parse("2"),
        },
    };

    std.testing.log_level = std.log.Level.info;
    for (cases, 1..) |c, index| {
        const code = c.code;
        const get = eval(code);
        try std.testing.expect(eq(get, c.want));
        std.log.info("test {}: ok", .{index});
    }

    {
        const code = "(defun double (x) (+ x x))";
        const get = eval(code);
        try std.testing.expectEqualStrings("double", get.function.name[0..]);
        std.log.info("test ok", .{});
    }
}

// defunをパースするとき -> その時のenvを渡し、functionオブジェクトで持つ
// defunを呼ぶとき -> その時のenvを渡さず、functionオブジェクトが持っているenvを使う

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
        Value.function => |aa| switch (b.*) {
            Value.function => |bb| return std.mem.eql(u8, aa.name, bb.name), // equality based on name
            else => return false,
        },
    }
}
