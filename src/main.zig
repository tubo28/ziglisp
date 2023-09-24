const std = @import("std");
const Map = std.StringHashMap(*const Value);

const common = @import("common.zig");
const nil = common.nil;
const toString = common.toString;
const alloc = common.alloc;
const Value = common.Value;

const T = @import("tokenize.zig");
const P = @import("parse.zig");
const E = @import("evaluate.zig");

pub fn main() !void {
    const args = try std.process.argsAlloc(common.alloc);
    if (args.len == 2) {
        try evalFile(args[1]);
        return;
    }
    try repl();
}

fn evalFile(filepath: []const u8) !void {
    const stdout = std.io.getStdOut().writer();

    var file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [65536]u8 = undefined;
    const size = try in_stream.readAll(&buf);
    const result = eval(buf[0..size]);
    try stdout.print("{s}\n", .{toString(result)});
}

fn repl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var env = Map.init(alloc);
    while (true) {
        try stdout.print(">>> ", .{});
        const line = readLine(stdin) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (line) |l| {
            if (l.len == 0) continue;
            const result = eval(l, &env);
            try stdout.print("{s}\n", .{toString(result)});
        }
    }
}

fn readLine(reader: anytype) !?[]const u8 {
    // Don't free buffer since it's referenced from slices in env entry.
    const len = 8192;
    var buffer: []u8 = try alloc.alloc(u8, len);
    var fbs = std.io.fixedBufferStream(buffer);
    try reader.streamUntilDelimiter(fbs.writer(), '\n', len);
    return fbs.getWritten();
}

fn eval(code: []const u8) *const Value {
    const tokens = T.tokenize(code);
    const sexprs = P.parse(tokens);
    var ret = nil();
    var env = &Map.init(common.alloc);
    for (sexprs) |expr| {
        ret, env = E.evaluate(expr, env);
    }
    // std.log.debug("eval result: {s}", .{tos(ret)});
    return ret;
}

fn parse(code: []const u8) []*const Value {
    const tokens = T.tokenize(code);
    const sexprs = P.parse(tokens);
    // for (sexprs) |expr| {
    //     std.log.debug("parse result: {s}", .{tos(expr)});
    // }
    return sexprs;
}

const M = std.AutoHashMap(i32, i32);

fn purePut(m: M, k: i32, v: i32) M {
    const new_m = m.clone();
    new_m.put(k, v) catch unreachable;
    return new_m;
}

test "test1" {
    var map1 = M.init(std.testing.allocator);
    defer map1.deinit();

    var map2 = map1;
    try map1.put(1, 100);

    try std.testing.expectEqual(
        @as(i32, 100),
        map1.get(1) orelse unreachable,
    );
    try std.testing.expectEqual(
        @as(i32, 100),
        map2.get(1) orelse unreachable, // <-- test fails because map2.get is null here
    );
}

test "test2" {
    var map1 = std.AutoHashMap(i32, i32).init(std.testing.allocator);
    defer map1.deinit();
    try map1.put(1, 100);

    var map2 = map1;
    try map2.put(2, 200);

    try std.testing.expectEqual(
        @as(i32, 100),
        map1.get(1) orelse unreachable,
    );
    try std.testing.expectEqual(
        @as(i32, 200),
        map1.get(2) orelse unreachable,
    );
    try std.testing.expectEqual(
        @as(i32, 100),
        map2.get(1) orelse unreachable,
    );
    try std.testing.expectEqual(
        @as(i32, 200),
        map2.get(2) orelse unreachable,
    );
}

test "tokenize" {
    const TestCase = struct {
        code: []const u8,
        want: []*const Value,
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
            .code = "(setq a 1) (+ a a)",
            .want = parse("2"),
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
            .code = "(progn (setq menu '(tea coffee milk)) (cons 'juice (cdr menu)))",
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
        TestCase{
            .code = "(progn (defun double (x) (+ x x)) (double (double 1)))",
            .want = parse("4"),
        },
        TestCase{
            .code = "(progn (defun double (x) (+ x x)) (double (double 1)))",
            .want = parse("4"),
        },
        TestCase{
            .code = "(if t 'true 'false)",
            .want = parse("true"),
        },
        TestCase{
            .code = "(if 0 'true 'false)",
            .want = parse("true"),
        },
        TestCase{
            .code = "(if nil 'true 'false)",
            .want = parse("false"),
        },
        TestCase{
            .code = "(if () 'true 'false)",
            .want = parse("false"),
        },
        TestCase{
            .code = "(if t 'true)",
            .want = parse("true"),
        },
        TestCase{
            .code = "(if nil 'true)",
            .want = parse("nil"),
        },
        TestCase{
            .code = @embedFile("examples/fibonacci.lisp"),
            .want = parse("89"),
        },
        TestCase{
            .code = "(let ((x 1) (y 2)) (+ 1 2))",
            .want = parse("3"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 0) 'bar))",
            .want = parse("bar"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) (t 'bar))",
            .want = parse("bar"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 2) 'bar))",
            .want = parse("nil"),
        },
        TestCase{
            .code = @embedFile("examples/mergesort.lisp"),
            .want = parse("(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)"),
        },
        TestCase{
            .code = @embedFile("examples/tarai.lisp"),
            .want = parse("8"),
        },
    };

    std.testing.log_level = std.log.Level.debug;
    for (cases, 1..) |c, i| {
        const code = c.code;
        std.log.debug("test {}: {s}", .{ i, code });
        const get = eval(code);
        try std.testing.expect(E.deepEql(get, c.want[c.want.len - 1]));
        std.log.info("test result: ok", .{});
    }
}
