const std = @import("std");

const T = @import("tok.zig").Token;
const S = @import("symbol.zig");
const C = @import("common.zig");
const Mem = @import("mem.zig");

const TokenKind = T.TokenKind;
const Value = C.Value;
const ValueRef = C.ValueRef;

const assert = std.debug.assert;
const Cons = C.newCons;

var define: ValueRef = undefined;
var lambda: ValueRef = undefined;
var let: ValueRef = undefined;

pub fn preprocessAll(trees: []ValueRef, result: []ValueRef) !void {
    define = try Mem.newValue(Value{ .symbol = try S.getOrRegister("define") });
    lambda = try Mem.newValue(Value{ .symbol = try S.getOrRegister("lambda") });
    let = try Mem.newValue(Value{ .symbol = try S.getOrRegister("let") });
    for (trees, 0..) |t, i| result[i] = try applyAllSteps(t);
}

fn applyAllSteps(tree: ValueRef) !ValueRef {
    var ret = tree;
    if (!step1(ret, 0, 0)) @panic("define is only allowed on top-level");
    ret = try step2(ret);
    ret = try step3(ret);
    return ret;
}

// Limit usage of 'define' in top-level only
fn step1(tree: ValueRef, depth: usize, nth_cdr: usize) bool {
    if (tree.* != .cons or tree == C.empty()) return true;

    if (tree.* == .symbol and tree.symbol == define.symbol)
        return depth == 0 and nth_cdr == 0;

    if (tree.* != .cons) return true;

    const car = tree.cons.car;
    const cdr = tree.cons.cdr;
    return true and
        step1(car, depth + 1, 0) and
        step1(cdr, depth, nth_cdr + 1);
}

// Replace (define (f a b ...) expr ...) to (define f [lambda (a b ...) expr ...])
fn step2(tree: ValueRef) !ValueRef {
    if (tree.* != .cons or tree == C.empty()) return tree;

    const car = tree.cons.car;
    const cdr = tree.cons.cdr;

    if (car.* != .symbol) return tree;
    if (car.symbol != define.symbol) return tree;

    const cadr = cdr.cons.car;
    const cddr = cdr.cons.cdr;

    if (cdr == C.empty() or cddr == C.empty()) @panic("insufficient params for define");
    //        std.log.err("{s}", .{try C.toString(tree)});
    if (cadr.* != .symbol and cadr.* != .cons) @panic("invalid define");

    if (cadr.* == .symbol) return tree; // (define x y)

    const f = cadr.cons.car; // f
    const params = cadr.cons.cdr; // a b ...
    const exprs = cddr; // expr ...

    const lambda_expr = try Cons(lambda, try Cons(params, exprs));
    const define_expr = try Cons(define, try Cons(f, try Cons(lambda_expr, C.empty())));
    return try step2(define_expr);
}

// Replace (let [(k v) ...] expr) to ((lambda (k ...) expr) v ...)
fn step3(tree: ValueRef) !ValueRef {
    if (tree.* != .cons or tree == C.empty()) return tree;

    const car = tree.cons.car;
    const cdr = tree.cons.cdr;

    if (car.* != .symbol or car.symbol != let.symbol)
        return try Cons(try step3(car), try step3(cdr));

    const cadr = cdr.cons.car;
    const cddr = cdr.cons.cdr;
    const caddr = cddr.cons.car;

    if (cdr == C.empty() or cddr == C.empty()) @panic("insufficient params for let");

    const kvs = cadr;
    const expr = caddr;

    var buf: [100]ValueRef = undefined;
    const kvs_slice = C.flattenToALU(kvs, &buf);

    const len = kvs_slice.items.len;

    var ary = try C.alloc.alloc(ValueRef, len * 2);
    defer C.alloc.free(ary);
    var ks = ary[0..len];
    var vs = ary[len..];

    for (kvs_slice.items, 0..) |kv, i| {
        ks[i] = kv.cons.car;
        vs[i] = kv.cons.cdr.cons.car;
    }

    const lambda_expr = try Cons(lambda, try Cons(try C.toConsList(ks), try Cons(expr, C.empty())));
    const apply_expr = try Cons(lambda_expr, try C.toConsList(vs));
    return try step3(apply_expr);
}
