const std = @import("std");

const C = @import("common.zig");
const E = @import("evaluate.zig");
const S = @import("symbol.zig");

const _caddr = C._caddr;
const _cadr = C._cadr;
const _car = C._car;
const _cdr = C._cdr;
const alloc = C.alloc;
const EvalResult = C.EvalResult;
const f = C.f;
const new = C.new;
const t = C.t;
const toSlice = C.toSlice;
const Value = C.Value;
const ValueRef = C.ValueRef;

const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;

pub const Function = fn (ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn (ValueRef, EnvRef) anyerror!EvalResult;

const func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "or", "and", "null?", "quotient", "modulo" };
pub const func = [_]*const Function{ car, cdr, cons_, list, print, add, sub, mul, eq, le, or_, and_, null_, quotient, modulo };

const spf_names = [_][]const u8{ "quote", "begin", "define", "lambda", "if", "cond", "let" };
pub const spf = [_]*const SpecialForm{ quote, begin, define, lambda, if_, cond, let };

pub fn loadBuiltin() !EnvRef {
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
fn let(args: ValueRef, env: *const Env) anyerror!EvalResult {
    const pairs = _car(args);
    const expr = _cadr(args);
    var buf: [100]ValueRef = undefined;
    const len = toSlice(pairs, &buf);
    const pairsSlice = buf[0..len];

    var new_binds = std.ArrayList(struct { S.ID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    for (pairsSlice) |p| {
        var keyVal: [2]ValueRef = undefined;
        const l = toSlice(p, &keyVal);
        std.debug.assert(l == 2);
        const k = keyVal[0].symbol;
        const v = keyVal[1];
        // No dependency between new values.
        const ev, _ = try E.evaluate(v, env);
        try new_binds.append(.{ k, ev });
    }

    const new_env = try env.overwrite(try new_binds.toOwnedSlice());
    return E.evaluate(expr, new_env);
}

// (define id expr)
fn defineValue() anyerror!ValueRef {
    unreachable;
}

// special form
fn quote(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    return .{ _car(args), env };
}

// special form
// Define function or value
fn define(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    std.debug.assert(_cdr(args) != C.empty()); // Ill-formed special form
    switch (_car(args).*) {
        Value.cons => return defineF(args, env),
        Value.symbol => return defineV(args, env),
        else => unreachable,
    }
}

// (define (name args) body ...+)
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
fn defineF(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    var buf: [100]ValueRef = undefined;
    const slice_len = toSlice(_car(args), &buf);
    const slice = buf[0..slice_len];

    var params = try alloc.alloc(S.ID, slice.len - 1);
    for (slice[1..], 0..) |arg, i| params[i] = arg.symbol;

    const name = slice[0].symbol;
    const body = _cdr(args);

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
fn defineV(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    const id = _car(args).symbol;
    const expr = _cadr(args);
    const val, _ = try E.evaluate(expr, env); // Assume that RHS has no side-effect.
    return .{ val, try env.overwriteOne(id, val) };
}

// special form
fn if_(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    const pred = _car(args);
    const then = _cadr(args);
    const unless = if (_caddr(args) != C.empty()) _caddr(args) else null;
    const p, const new_env = try E.evaluate(pred, env);
    if (toBool(p)) return try E.evaluate(then, new_env);
    if (unless) |u| return try E.evaluate(u, new_env);
    return .{ C.empty(), new_env }; // Return empty if pred is false and unless is not given.
}

// special form
fn cond(clauses: ValueRef, env: EnvRef) anyerror!EvalResult {
    var e = env;
    var h = clauses;
    while (h != C.empty()) {
        const cl = _car(h);
        const pred = _car(cl);
        const p, e = try E.evaluate(pred, e);
        if (toBool(p)) return E.evaluate(_cadr(cl), e);
        h = _cdr(h);
    }
    return .{ C.empty(), e }; // Return empty if all pred is false.
}

// special form
fn begin(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    var ret = C.empty();
    var new_env = env;

    var h = args;
    while (h != C.empty()) {
        const x = _car(h);
        ret, new_env = try E.evaluate(x, new_env);
        h = _cdr(h);
    }
    return .{ ret, new_env }; // return the last result
}

// built-in func
fn car(args: ValueRef) anyerror!ValueRef {
    return _car(_car(args));
}

// built-in func
fn cdr(args: ValueRef) anyerror!ValueRef {
    return _cdr(_car(args));
}

// built-in func
fn cons_(args: ValueRef) anyerror!ValueRef {
    return C.newCons(_car(args), _cadr(args));
}

// built-in func
fn list(xs: ValueRef) anyerror!ValueRef {
    if (xs == C.empty()) return xs;
    return C.newCons(_car(xs), try list(_cdr(xs)));
}

// built-in func
fn add(xs: ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    var h = xs;
    while (h != C.empty()) {
        const x = _car(h);
        ret += x.number;
        h = _cdr(h);
    }
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn sub(xs: ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    var h = xs;
    while (h != C.empty()) {
        const x = _car(h);
        ret += if (h == xs) x.number else -x.number;
        h = _cdr(h);
    }
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn mul(xs: ValueRef) anyerror!ValueRef {
    var ret: i64 = 1;
    var h = xs;
    while (h != C.empty()) {
        const x = _car(h);
        ret *= x.number;
        h = _cdr(h);
    }
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn quotient(xs: ValueRef) anyerror!ValueRef {
    const ret = @divFloor(_car(xs).number, _cadr(xs).number);
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn modulo(xs: ValueRef) anyerror!ValueRef {
    const ret = @mod(_car(xs).number, _cadr(xs).number);
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn or_(xs: ValueRef) anyerror!ValueRef {
    var h = xs;
    while (h != C.empty()) {
        const x = _car(h);
        if (toBool(x)) return t();
        h = _cdr(h);
    }
    return f();
}

// built-in func
fn and_(xs: ValueRef) anyerror!ValueRef {
    var h = xs;
    while (h != C.empty()) {
        const x = _car(h);
        if (!toBool(x)) return f();
        h = _cdr(h);
    }
    return t();
}

// built-in func
fn eq(args: ValueRef) anyerror!ValueRef {
    if (C.deepEql(_car(args), _cadr(args))) return t();
    return f();
}

fn toValue(x: bool) anyerror!ValueRef {
    return if (x) t() else f();
}

// built-in func
fn le(args: ValueRef) anyerror!ValueRef {
    return toValue(_car(args).number < _cadr(args).number);
}

// built-in func
fn null_(x: ValueRef) anyerror!ValueRef {
    return toValue(_car(x) == C.empty());
}

fn toBool(x: ValueRef) bool {
    return !isF(x);
}

fn isF(x: ValueRef) bool {
    return x == f();
}

// built-in func
fn print(xs: ValueRef) !ValueRef {
    var h = xs;
    var ret = C.empty();
    while (h != C.empty()) {
        const x = _car(h);
        ret = x;
        const str = try C.toString(x);
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
        h = _cdr(h);
    }
    return ret;
}

// special form
// (lambda (x y) (+ x y))
fn lambda(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    const params = _car(args);
    const body = _cdr(args);
    var tmp: [100]ValueRef = undefined;
    const len = toSlice(params, &tmp);
    const val_params = tmp[0..len];
    var sym_params = try alloc.alloc(S.ID, val_params.len);
    for (val_params, 0..) |a, i| sym_params[i] = a.symbol;
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
