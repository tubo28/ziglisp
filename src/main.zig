const std = @import("std");
const Map = std.StringHashMap(*const Value);

const common = @import("common.zig");
const nil = common.nil;
const tos = common.toStringDot;
const alloc = common.alloc;
const Value = common.Value;

const T = @import("tokenize.zig");
const P = @import("parse.zig");
const E = @import("evaluate.zig");

pub fn main() void {
    std.debug.print("Hello, {s}!\n", .{"World"});
}

fn eval(code: []const u8) *const Value {
    const tokens = T.tokenize(code);
    const sexpr = P.parse(tokens);
    var env = Map.init(alloc);
    const value = E.evaluate(sexpr.value, &env);
    std.log.debug("eval result: {s}", .{tos(value)});
    return value;
}

fn parse(code: []const u8) *const Value {
    const tokens = T.tokenize(code);
    const sexpr = P.parse(tokens);
    std.log.debug("parse result: {s}", .{tos(sexpr.value)});
    return sexpr.value;
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
    };

    std.testing.log_level = std.log.Level.debug;
    for (cases, 1..) |c, i| {
        const code = c.code;
        std.log.debug("test {}: {s}", .{ i, code });
        const get = eval(code);
        try std.testing.expect(eq(get, c.want));
        std.log.info("test result: ok", .{});
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
        Value.function => |aa| switch (b.*) {
            Value.function => |bb| return std.mem.eql(u8, aa.name, bb.name), // equality based on name
            else => return false,
        },
    }
}
