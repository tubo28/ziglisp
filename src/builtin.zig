const std = @import("std");

const C = @import("common.zig");
const E = @import("evaluate.zig");
const S = @import("symbol.zig");

const _caddr = C._caddr;
const _cadr = C._cadr;
const _car = C._car;
const _cdr = C._cdr;
const _cddr = C._cddr;
const alloc = C.alloc;
const EvalResult = C.EvalResult;
const f = C.f;
const new = C.new;
const t = C.t;
const toSlice = C.toSlice;
const Value = C.Value;
const ValueRef = C.ValueRef;
const Macro = C.Macro;
const MacroRule = C.MacroRule;

const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;

pub const Function = fn ([]ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn ([]ValueRef, EnvRef) anyerror!EvalResult;

const func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "or", "and", "null?", "quotient", "modulo" };
pub const func = [_]*const Function{ car, cdr, cons_, list, print, add, sub, mul, eq, le, or_, and_, null_, quotient, modulo };

const spf_names = [_][]const u8{ "quote", "begin", "define", "lambda", "if", "cond", "let", "define-syntax" };
pub const spf = [_]*const SpecialForm{ quote, begin, define, lambda, if_, cond, let, defineSyntax };

pub fn loadBuiltin() !EnvRef {
    std.debug.assert(func_names.len == func.len);
    std.debug.assert(spf_names.len == spf.len);

    // Bind symbol and symbol id
    for (func_names, 100_000_000..) |name, sid|
        try S.registerUnsafe(name, sid);
    for (spf_names, 200_000_000..) |name, sid|
        try S.registerUnsafe(name, sid);

    // Bind symbol and function
    var new_binds = std.ArrayList(struct { S.ID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    for (100_000_000.., 0..func.len) |sid, i|
        try new_binds.append(.{ sid, try new(Value, Value{ .b_func = i }) });
    for (200_000_000.., 0..spf.len) |sid, i|
        try new_binds.append(.{ sid, try new(Value, Value{ .b_spf = i }) });

    const ret = try Env.new();
    return ret.overwrite(try new_binds.toOwnedSlice());
}

// special form
fn let(args: []ValueRef, env: *const Env) anyerror!EvalResult {
    std.debug.assert(args.len == 2);
    const pairs = args[0];
    var buf: [100]ValueRef = undefined;
    const pairsSlice = toSlice(pairs, &buf);

    var new_binds = std.ArrayList(struct { S.ID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    for (pairsSlice) |p| {
        var tmp: [2]ValueRef = undefined;
        const keyVal = toSlice(p, &tmp);
        std.debug.assert(keyVal.len == 2);
        const k = keyVal[0].symbol;
        const v = keyVal[1];
        // No dependency between new values.
        const ev, _ = try E.evaluate(v, env);
        try new_binds.append(.{ k, ev });
    }

    const new_env = try env.overwrite(try new_binds.toOwnedSlice());
    const expr = args[1];
    return E.evaluate(expr, new_env);
}

// (define id expr)
fn defineValue() anyerror!ValueRef {
    unreachable;
}

// special form
fn quote(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    return .{ args[0], env };
}

// special form
// Define function or value
fn define(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    std.debug.assert(args.len > 0);
    switch (args[0].*) {
        Value.cons => return defineF(args, env),
        Value.symbol => return defineV(args, env),
        else => unreachable,
    }
}

// (define (name args) body ...+)
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
fn defineF(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const name, const params, const body = b: {
        var buf: [100]ValueRef = undefined;
        const slice = toSlice(args[0], &buf);
        const name = slice[0].symbol;
        const params = try alloc.alloc(S.ID, slice.len - 1);
        for (slice[1..], 0..) |arg, i| params[i] = arg.symbol;

        const body = try alloc.alloc(ValueRef, args.len - 1);
        std.mem.copy(ValueRef, body, args[1..]);
        break :b .{ name, params, body };
    };

    var ret = try new(Value, undefined);
    const new_env = try env.overwriteOne(name, ret);
    const func_val = try new(C.Function, C.Function{
        .name = name,
        .params = params,
        .body = undefined,
        .env = new_env,
    });

    // TODO: Apply alpha conversion to body with new_env
    func_val.body = body;

    ret.* = Value{ .function = func_val };
    return .{ ret, new_env };
}

// (define id expr)
fn defineV(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const id = args[0].symbol;
    const expr = args[1];
    const val, _ = try E.evaluate(expr, env); // Assume that RHS has no side-effect.
    return .{ val, try env.overwriteOne(id, val) };
}

// special form
fn if_(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const pred = args[0];
    const then = args[1];
    const unless = if (args.len >= 3) args[2] else null;
    const p, const new_env = try E.evaluate(pred, env);
    if (toBool(p)) return try E.evaluate(then, new_env);
    if (unless) |u| return try E.evaluate(u, new_env);
    return .{ C.empty(), new_env }; // Return empty if pred is false and unless is not given.
}

// special form
fn cond(clauses: []ValueRef, env: EnvRef) anyerror!EvalResult {
    var e = env;
    for (clauses) |cl| {
        const pred = _car(cl);
        const p, e = try E.evaluate(pred, e);
        if (toBool(p)) return E.evaluate(_cadr(cl), e);
    }
    return .{ C.empty(), e }; // Return empty if all pred is false.
}

// special form
fn begin(exprs: []ValueRef, env: EnvRef) anyerror!EvalResult {
    var ret = C.empty();
    var new_env = env;
    for (exprs) |expr| ret, new_env = try E.evaluate(expr, new_env);
    return .{ ret, new_env }; // return the last result
}

// built-in func
fn car(args: []ValueRef) anyerror!ValueRef {
    return _car(args[0]);
}

// built-in func
fn cdr(args: []ValueRef) anyerror!ValueRef {
    return _cdr(args[0]);
}

// built-in func
fn cons_(args: []ValueRef) anyerror!ValueRef {
    return C.newCons(args[0], args[1]);
}

// built-in func
fn list(xs: []ValueRef) anyerror!ValueRef {
    if (xs.len == 0) return C.empty();
    return C.newCons(xs[0], try list(xs[1..]));
}

// built-in func
fn add(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs) |x| ret += x.number;
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn sub(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| ret += if (i == 0) x.number else -x.number;
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn mul(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 1;
    for (xs) |x| ret *= x.number;
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn quotient(xs: []ValueRef) anyerror!ValueRef {
    const ret = @divFloor(xs[0].number, xs[1].number);
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn modulo(xs: []ValueRef) anyerror!ValueRef {
    const ret = @mod(xs[0].number, xs[1].number);
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn or_(xs: []ValueRef) anyerror!ValueRef {
    for (xs) |x| if (toBool(x)) return t();
    return f();
}

// built-in func
fn and_(xs: []ValueRef) anyerror!ValueRef {
    for (xs) |x| if (!toBool(x)) return f();
    return t();
}

// built-in func
fn eq(xs: []ValueRef) anyerror!ValueRef {
    if (C.deepEql(xs[0], xs[1])) return t();
    return f();
}

fn toValue(x: bool) anyerror!ValueRef {
    return if (x) t() else f();
}

// built-in func
fn le(xs: []ValueRef) anyerror!ValueRef {
    return toValue(xs[0].number < xs[1].number);
}

// built-in func
fn null_(xs: []ValueRef) anyerror!ValueRef {
    return toValue(xs[0] == C.empty());
}

fn toBool(x: ValueRef) bool {
    return !isF(x);
}

fn isF(x: ValueRef) bool {
    return x == f();
}

// built-in func
fn print(xs: []ValueRef) !ValueRef {
    if (xs.len == 0) return C.empty();
    for (xs) |x| {
        const str = try C.toString(x);
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
    }
    return xs[xs.len - 1];
}

// special form
// (lambda (x y) (+ x y))
fn lambda(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    std.debug.assert(args.len >= 2);
    const params = args[0];
    const body = try alloc.alloc(ValueRef, args.len - 1);
    std.mem.copy(ValueRef, body, args[1..]);
    // Convert ValueRefs to symbols
    var sym_params = b: {
        var tmp: [100]ValueRef = undefined;
        const val_params = toSlice(params, &tmp);
        var ret = try alloc.alloc(S.ID, val_params.len);
        for (val_params, 0..) |a, i| ret[i] = a.symbol;
        break :b ret;
    };
    const func_val = try C.new(Value, Value{
        .function = try new(C.Function, C.Function{
            .name = null,
            .params = sym_params,
            .body = body,
            .env = env,
        }),
    });
    return .{ func_val, env };
}

// (define-syntax id expr)
// (syntax-rules (literal-id ...)
//   ((id pattern) template) ...)

// (define-syntax if
//   (syntax-rules ()
//     ((_ test then else)
//      (cond (test then)
//            (else else)))))
// (define-syntax incf
//   (syntax-rules ()
//     ((_ x) (begin (set! x (+ x 1)) x))
//     ((_ x i) (begin (set! x (+ x i)) x))))
fn defineSyntax(lst: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const name = lst[0].symbol;
    const m = try new(Macro, Macro{
        .name = name,
        .rules = try parseRules(lst[1]),
    });
    const macro = try new(Value, Value{ .macro = m });
    return .{ macro, try env.overwriteOne(name, macro) };
}

fn parseRules(lst: ValueRef) ![]MacroRule {
    const syntax_rules = try S.getOrRegister("syntax-rules");

    std.debug.assert(_car(lst).symbol == syntax_rules);
    std.debug.assert(_cadr(lst) == C.empty()); // TODO

    var tmp: [100]ValueRef = undefined;
    const rules = toSlice(_cddr(lst), &tmp);

    var ret = try alloc.alloc(MacroRule, rules.len);
    for (rules, 0..) |r, i|
        ret[i] = MacroRule{ .pattern = _car(r), .template = _cadr(r) };
    return ret;
}
