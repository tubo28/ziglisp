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
    if (!tree.isCons() or tree.isEmpty()) return true;

    if (tree.isSymbol() and tree.symbol() == define.symbol())
        return depth == 0 and nth_cdr == 0;

    if (!tree.isCons()) return true;

    const car = tree.car();
    const cdr = tree.cdr();
    return true and
        step1(car, depth + 1, 0) and
        step1(cdr, depth, nth_cdr + 1);
}

// Replace (define (f a b ...) expr ...) to (define f [lambda (a b ...) expr ...])
fn step2(tree: ValueRef) !ValueRef {
    if (!tree.isCons() or tree.isEmpty()) return tree;

    const car = tree.car();
    const cdr = tree.cdr();

    if (!car.isSymbol()) return tree;
    if (car.symbol() != define.symbol()) return tree;

    const cadr = cdr.car();
    const cddr = cdr.cdr();

    if (cdr.isEmpty() or cddr.isEmpty()) @panic("insufficient params for define");
    //        std.log.err("{s}", .{try C.toString(tree)});
    if (!cadr.isSymbol() and !cadr.isCons()) @panic("invalid define");

    if (cadr.isSymbol()) return tree; // (define x y)

    const f = cadr.car(); // f
    const params = cadr.cdr(); // a b ...
    const exprs = cddr; // expr ...

    const lambda_expr = try Cons(lambda, try Cons(params, exprs));
    const define_expr = try Cons(define, try Cons(f, try Cons(lambda_expr, C.empty())));
    return try step2(define_expr);
}

// Replace (let [(k v) ...] expr) to ((lambda (k ...) expr) v ...)
fn step3(tree: ValueRef) !ValueRef {
    if (!tree.isCons() or tree.isEmpty()) return tree;

    const car = tree.car();
    const cdr = tree.cdr();

    if (!car.isSymbol() or car.symbol() != let.symbol())
        return try Cons(try step3(car), try step3(cdr));

    const cadr = cdr.car();
    const cddr = cdr.cdr();
    const caddr = cddr.car();

    if (cdr.isEmpty() or cddr.isEmpty()) @panic("insufficient params for let");

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
        ks[i] = kv.car();
        vs[i] = kv.cdr().car();
    }

    const lambda_expr = try Cons(lambda, try Cons(try C.toConsList(ks), try Cons(expr, C.empty())));
    const apply_expr = try Cons(lambda_expr, try C.toConsList(vs));
    return try step3(apply_expr);
}
