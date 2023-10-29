const std = @import("std");
const M = @import("main.zig");
const C = @import("common.zig");

test "batch" {
    std.testing.log_level = std.log.Level.debug;

    const TestCase = struct {
        code: []const u8,
        want: []const u8,
    };

    const cases = [_]TestCase{
        TestCase{
            .code = "1",
            .want = "1",
        },
        TestCase{
            .code = "a",
            .want = "a",
        },
        TestCase{
            .code = "(+ 1 2)",
            .want = "3",
        },
        TestCase{
            .code = "(+ 1 2 (+ 3 4) (+ 5 (+ 6 7)) 8 9 10)",
            .want = "55",
        },
        TestCase{
            .code = "'(1 2 3)",
            .want = "(1 2 3)",
        },
        TestCase{
            .code = "(< 1 2)",
            .want = "#t",
        },
        TestCase{
            .code = "(> 2 1)",
            .want = "#t",
        },
        TestCase{
            .code = "(length '(1 2 3))",
            .want = "3",
        },
        TestCase{
            .code = "(+ (length '(a b c)) (length '(d e)))",
            .want = "5",
        },
        TestCase{
            .code = "(print hello)",
            .want = "hello",
        },
        TestCase{
            .code = "(begin (print hello) (print world) (+ (length '(a b c)) (length '(d e))))",
            .want = "5",
        },
        TestCase{
            .code = "(car '(a b c))",
            .want = "a",
        },
        TestCase{
            .code = "(car '(a . b))",
            .want = "a",
        },
        TestCase{
            .code = "(cdr '(a . b))",
            .want = "b",
        },
        TestCase{
            .code = "(car '(a b . c))",
            .want = "a",
        },
        TestCase{
            .code = "(cdr '(a b . c))",
            .want = "(b . c)",
        },
        TestCase{
            .code = "(car '((a b) (c d)))",
            .want = "(a b)",
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (car menu))",
            .want = "tea",
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (cdr menu))",
            .want = "(coffee milk)",
        },
        TestCase{
            .code = "(let ((menu '(tea coffee milk))) (cdr (cdr menu)))",
            .want = "(milk)",
        },
        TestCase{
            .code = "(define x 1) (define y (+ x 1)) (+ x y)",
            .want = "3",
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double 1)",
            .want = "2",
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double (double 1))",
            .want = "4",
        },
        TestCase{
            .code = "(define (double x) (+ x x)) (double (double 1))",
            .want = "4",
        },
        TestCase{
            .code = "(if t 'true 'false)",
            .want = "true",
        },
        TestCase{
            .code = "(if 0 'true 'false)",
            .want = "true",
        },
        TestCase{
            .code = "(if #f 'true 'false)",
            .want = "false",
        },
        TestCase{
            .code = "(if #t 'true)",
            .want = "true",
        },
        TestCase{
            .code = "(if #f 'true)",
            .want = "()",
        },
        TestCase{
            .code = @embedFile("examples/fibonacci.scm"),
            .want = "89",
        },
        TestCase{
            .code = "(let ((x 1) (y 2)) (+ 1 2))",
            .want = "3",
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 0) 'bar))",
            .want = "bar",
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) (t 'bar))",
            .want = "bar",
        },
        TestCase{
            .code = "(cond ((= 0 1) 'foo) ((= 0 2) 'bar))",
            .want = "()",
        },
        TestCase{
            .code = @embedFile("examples/mergesort.scm"),
            .want = "(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)",
        },
        TestCase{
            .code = @embedFile("examples/quicksort.scm"),
            .want = "(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)",
        },
        TestCase{
            .code = @embedFile("examples/tarai.scm"),
            .want = "6",
        },
        TestCase{
            .code = "(let ((f (lambda (x) (+ x x)))) (f 1))",
            .want = "2",
        },
        TestCase{
            .code = "((lambda (x) (+ x x)) 1)",
            .want = "2",
        },
        TestCase{
            .code = @embedFile("examples/y-comb.scm"),
            .want = "55",
        },
        TestCase{
            .code = @embedFile("examples/y-comb2.scm"),
            .want = "(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946)",
        },
        TestCase{
            .code = "((lambda xs (sum xs)) 1 2 3 4 5)",
            .want = "15",
        },
        TestCase{
            .code = "((lambda (x . xs) (+ x (sum xs))) 1 2 3 4 5)",
            .want = "15",
        },
        TestCase{
            .code = "((lambda (x y . xs) (+ x y (sum xs))) 1 2 3 4 5)",
            .want = "15",
        },
        TestCase{
            .code = "(define (my-list . x) x) (my-list 1 2 3 4 5)",
            .want = "(1 2 3 4 5)",
        },
        TestCase{
            .code = "(define (my-list x . xs) xs) (my-list 1 2 3 4 5)",
            .want = "(2 3 4 5)",
        },
        TestCase{
            .code = "(define (my-list x y . xs) xs) (my-list 1 2 3 4 5)",
            .want = "(3 4 5)",
        },
    };

    for (cases, 1..) |c, i| {
        const code = c.code;
        std.log.debug("test {}: {s}", .{ i, code });
        const env = try M.init();
        const tmp = try M.toAST(c.want);
        const want = tmp[tmp.len - 1];
        const get, _ = try M.eval(code, env);
        if (!C.deepEql(get, want)) {
            std.log.debug("{s} is not {s}", .{ try C.toString(get), try C.toString(want) });
            try std.testing.expect(false);
        }
    }
}
