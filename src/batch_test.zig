const std = @import("std");
const M = @import("main.zig");
const C = @import("common.zig");

test "batch" {
    const toAST = M.toAST;
    std.testing.log_level = std.log.Level.debug;

    const env = try M.init();
    const TestCase = struct {
        code: []const u8,
        want: []C.ValueRef,
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
            .code = "(< 1 2)",
            .want = try toAST("#t"),
        },
        TestCase{
            .code = "(> 2 1)",
            .want = try toAST("#t"),
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
            .code = "(car '(a . b))",
            .want = try toAST("a"),
        },
        TestCase{
            .code = "(cdr '(a . b))",
            .want = try toAST("b"),
        },
        TestCase{
            .code = "(car '(a b . c))",
            .want = try toAST("a"),
        },
        TestCase{
            .code = "(cdr '(a b . c))",
            .want = try toAST("(b . c)"),
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
            .code = "(define (x) 1) (x)",
            .want = try toAST("1"),
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double 1)",
            .want = try toAST("2"),
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double (double 1))",
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
        const get, _ = try M.eval(code, env);
        try std.testing.expect(C.deepEql(get, c.want[c.want.len - 1]));
        std.log.info("test result: ok", .{});
    }
}
