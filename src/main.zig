const std = @import("std");

const B = @import("builtin.zig");
const C = @import("common.zig");
const E = @import("evaluate.zig");
const P = @import("parse.zig");
const M = @import("macro.zig");
const S = @import("symbol.zig");
const T = @import("tokenize.zig");

const Env = @import("env.zig").Env;
const alloc = C.alloc;
const EnvRef = Env.Ref;
const toString = C.toString;
const ValueRef = C.ValueRef;

pub fn main() !void {
    const args = try std.process.argsAlloc(C.alloc);
    var env = try init();
    if (args.len == 2) {
        try evalFile(args[1], env);
        return;
    }
    try repl(env);
}

fn init() !EnvRef {
    try S.init();
    _, const ret = try eval(@embedFile("builtin.scm"), try B.loadBuiltin());
    return ret;
}

fn evalFile(filepath: []const u8, env: EnvRef) !void {
    var file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [65536]u8 = undefined;
    const size = try in_stream.readAll(&buf);
    _ = try eval(buf[0..size], env);
}

fn repl(env: EnvRef) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var e = env;
    while (true) {
        try stdout.print(">>> ", .{});
        const line = readLine(stdin) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (line) |l| {
            if (l.len == 0) continue;
            const result, e = try eval(l, e);
            try stdout.print("{s}\n", .{try toString(result)});
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

fn eval(code: []const u8, env: EnvRef) !struct { ValueRef, EnvRef } {
    const sexprs = try toAST(code);
    var ret = C.empty();
    var new_env = env;
    for (sexprs) |expr| ret, new_env = try E.evaluate(expr, new_env);
    return .{ ret, new_env };
}

fn toAST(code: []const u8) ![]ValueRef {
    const tokens = try T.tokenize(code);
    const sexprs = try P.parse(tokens);
    return M.preproc(sexprs);
}

test "tokenize" {
    std.testing.log_level = std.log.Level.debug;
    const env = try init();
    const TestCase = struct {
        code: []const u8,
        want: []ValueRef,
    };

    const cases = [_]TestCase{
        TestCase{
            .code = "(+ 1 2)",
            .want = try toAST("3"),
        },
        TestCase{
            .code = "(+ 1 2 (+ 3 4) (+ 5 (+ 6 7)) 8 9 10)",
            .want = try toAST("55"),
        },
        TestCase{
            .code = "'(1 2 3)",
            .want = try toAST("(1 2 3)"),
        },
        TestCase{
            .code = "(length '(1 2 3))",
            .want = try toAST("3"),
        },
        TestCase{
            .code = "(+ (length '(a b c)) (length '(d e)))",
            .want = try toAST("5"),
        },
        TestCase{
            .code = "(print hello)",
            .want = try toAST("hello"),
        },
        TestCase{
            .code = "(begin (print hello) (print world) (+ (length '(a b c)) (length '(d e))))",
            .want = try toAST("5"),
        },
        TestCase{
            .code = "(car '(a b c))",
            .want = try toAST("a"),
        },
        TestCase{
            .code = "(car '((a b) (c d)))",
            .want = try toAST("(a b)"),
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (car menu))",
            .want = try toAST("tea"),
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (cdr menu))",
            .want = try toAST("(coffee milk)"),
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (cdr (cdr menu)))",
            .want = try toAST("(milk)"),
        },
        TestCase{
            .code = "(define x 1) (define y (+ x 1)) (+ x y)",
            .want = try toAST("3"),
        },
        TestCase{
            .code = "(begin (define (x) 1) (x))",
            .want = try toAST("1"),
        },
        TestCase{
            .code = "(begin (define (double x) (+ x x)) (double 1))",
            .want = try toAST("2"),
        },
        TestCase{
            .code = "(begin (define (double x) (+ x x)) (double (double 1)))",
            .want = try toAST("4"),
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double (double 1))",
            .want = try toAST("4"),
        },
        TestCase{
            .code = "(if t 'true 'false)",
            .want = try toAST("true"),
        },
        TestCase{
            .code = "(if 0 'true 'false)",
            .want = try toAST("true"),
        },
        TestCase{
            .code = "(if #f 'true 'false)",
            .want = try toAST("false"),
        },
        TestCase{
            .code = "(if #t 'true)",
            .want = try toAST("true"),
        },
        TestCase{
            .code = "(if #f 'true)",
            .want = try toAST("()"),
        },
        TestCase{
            .code = @embedFile("examples/fibonacci.scm"),
            .want = try toAST("89"),
        },
        TestCase{
            .code = "(let ((x 1) (y 2)) (+ 1 2))",
            .want = try toAST("3"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 0) 'bar))",
            .want = try toAST("bar"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) (t 'bar))",
            .want = try toAST("bar"),
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 2) 'bar))",
            .want = try toAST("()"),
        },
        TestCase{
            .code = @embedFile("examples/mergesort.scm"),
            .want = try toAST("(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)"),
        },
        TestCase{
            .code = @embedFile("examples/quicksort.scm"),
            .want = try toAST("(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)"),
        },
        TestCase{
            .code = @embedFile("examples/tarai.scm"),
            .want = try toAST("6"),
        },
        TestCase{
            .code = "(let ((f (lambda (x) (+ x x)))) (f 1))",
            .want = try toAST("2"),
        },
        TestCase{
            .code = "((lambda (x) (+ x x)) 1)",
            .want = try toAST("2"),
        },
        TestCase{
            .code = @embedFile("examples/y-comb.scm"),
            .want = try toAST("55"),
        },
        TestCase{
            .code = @embedFile("examples/y-comb2.scm"),
            .want = try toAST("(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946)"),
        },
    };

    for (cases, 1..) |c, i| {
        const code = c.code;
        std.log.debug("test {}: {s}", .{ i, code });
        const get, _ = try eval(code, env);
        try std.testing.expect(C.deepEql(get, c.want[c.want.len - 1]));
        std.log.info("test result: ok", .{});
    }
}
