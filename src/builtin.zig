const std = @import("std");
const common = @import("common.zig");
const Symbol = @import("symbol.zig");
const Evaluate = @import("evaluate.zig");

const alloc = common.alloc;
const EvalResult = common.EvalResult;
const f = common.f;
const t = common.t;
const toSlice = common.toSlice;
const Value = common.Value;
const ValueRef = common.ValueRef;

const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;

pub const Function = fn ([]ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn ([]ValueRef, EnvRef) anyerror!EvalResult;

const func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "or", "and", "null?", "quotient", "modulo" };
pub const func = [_]*const Function{ car, cdr, cons_, list, print, add, sub, mul, eq, le, or_, and_, null_, quotient, modulo };

const spf_names = [_][]const u8{ "quote", "begin", "define", "lambda", "if", "cond", "let" };
pub const spf = [_]*const SpecialForm{ quote, begin, defineFunction, lambda, if_, cond, let };

pub fn loadBuiltin() !EnvRef {
    // Bind symbol and symbol id
    for (func_names, 100_000_000..) |name, sid|
        try Symbol.registerUnsafe(name, sid);
    for (spf_names, 200_000_000..) |name, sid|
        try Symbol.registerUnsafe(name, sid);

    // Bind symbol and function
    var new_binds = std.ArrayList(struct { Symbol.ID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    for (100_000_000.., 0..func.len) |sid, i|
        try new_binds.append(.{ sid, try common.newBFunctionValue(i) });
    for (200_000_000.., 0..spf.len) |sid, i|
        try new_binds.append(.{ sid, try common.newBSpecialForm(i) });

    const ret = try Env.new();
    return ret.overwrite(try new_binds.toOwnedSlice());
}

// special form
fn let(args: []ValueRef, env: *const Env) anyerror!EvalResult {
    const pairs = args[0];
    const expr = args[1];
    const pairsSlice = try toSlice(pairs);

    var new_binds = std.ArrayList(struct { Symbol.ID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    for (pairsSlice) |p| {
        const keyVal = try toSlice(p);
        const k = keyVal[0].symbol;
        const v = keyVal[1];
        // No dependency between new values.
        const ev, _ = try Evaluate.evaluate(v, env);
        try new_binds.append(.{ k, ev });
    }

    const new_env = try env.overwrite(try new_binds.toOwnedSlice());
    return Evaluate.evaluate(expr, new_env);
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
// (define (name args) body ...+)
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
fn defineFunction(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const params = args[0];
    const body = args[1..];
    std.debug.assert(body.len != 0); // Ill-formed special form
    const slice = try toSlice(params);
    const name = slice[0];

    var sym_params = try alloc.alloc(Symbol.ID, slice.len - 1);
    for (slice[1..], 0..) |arg, i| sym_params[i] = arg.symbol;

    const func_val = try common.newFunctionValue(try common.newFunction(
        name.symbol,
        sym_params,
        body,
        env,
    ));
    return .{ func_val, try env.overwriteOne(name.symbol, func_val) };
}

// special form
fn if_(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const pred = args[0];
    const then = args[1];
    const unless = if (args.len >= 3) args[2] else null;
    const p, const new_env = try Evaluate.evaluate(pred, env);
    if (toBool(p)) return try Evaluate.evaluate(then, new_env);
    if (unless) |u| return try Evaluate.evaluate(u, new_env);
    return .{ common.empty(), new_env }; // Return empty if pred is false and unless is not given.
}

// special form
fn cond(clauses: []ValueRef, env: EnvRef) anyerror!EvalResult {
    var e = env;
    for (clauses) |c| {
        const tmp = try toSlice(c);
        const pred = tmp[0];
        const then = tmp[1];
        const p, e = try Evaluate.evaluate(pred, e);
        if (toBool(p)) return Evaluate.evaluate(then, e);
    }
    return .{ common.empty(), e }; // Return empty if all pred is false.
}

// special form
fn begin(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    var ret = common.empty();
    var new_env = env;
    for (args) |p| ret, new_env = try Evaluate.evaluate(p, new_env);
    return .{ ret, new_env }; // return the last result
}

// built-in func
fn car(args: []ValueRef) anyerror!ValueRef {
    return args[0].cons.car;
}

// built-in func
fn cdr(args: []ValueRef) anyerror!ValueRef {
    return args[0].cons.cdr;
}

// built-in func
fn cons_(args: []ValueRef) anyerror!ValueRef {
    return common.newConsValue(args[0], args[1]);
}

// built-in func
fn list(xs: []ValueRef) anyerror!ValueRef {
    var ret = common.empty();
    var i = xs.len;
    while (i > 0) {
        i -= 1;
        ret = try common.newConsValue(xs[i], ret);
    }
    return ret;
}

// built-in func
fn add(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs) |x| ret += x.number;
    return common.newNumberValue(ret);
}

// built-in func
fn sub(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| {
        if (i == 0) ret += x.number else ret -= x.number;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn mul(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= x.number;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn quotient(xs: []ValueRef) anyerror!ValueRef {
    const a = xs[0].number;
    const b = xs[1].number;
    return common.newNumberValue(@divFloor(a, b));
}

// built-in func
fn modulo(xs: []ValueRef) anyerror!ValueRef {
    const a = xs[0].number;
    const b = xs[1].number;
    return common.newNumberValue(@mod(a, b));
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
fn eq(args: []ValueRef) anyerror!ValueRef {
    if (common.deepEql(args[0], args[1])) return t();
    return f();
}

fn toValue(x: bool) anyerror!ValueRef {
    return if (x) t() else f();
}

// built-in func
fn le(args: []ValueRef) anyerror!ValueRef {
    return toValue(args[0].number < args[1].number);
}

// built-in func
fn null_(x: []ValueRef) anyerror!ValueRef {
    return toValue(x[0] == common.empty());
}

fn toBool(x: ValueRef) bool {
    return !isF(x);
}

fn isF(x: ValueRef) bool {
    return x == f();
}

// built-in func
fn print(xs: []ValueRef) !ValueRef {
    for (xs) |x| {
        const str = try common.toString(x);
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
    }
    return xs[xs.len - 1];
}

// special form
fn lambda(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    const params = args[0];
    const body = args[1..];
    var val_params = try toSlice(params);
    var sym_params = try alloc.alloc(Symbol.ID, val_params.len);
    for (val_params, 0..) |a, i| sym_params[i] = a.symbol;
    const func_val = try common.newFunctionValue(try common.newFunction(
        null,
        sym_params,
        body,
        env,
    ));
    return .{ func_val, env };
}
