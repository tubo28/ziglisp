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

// TODO: Change argument list from ValueRef (cons list) to []ValueRef
pub const Function = fn (ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn (ValueRef, EnvRef) anyerror!EvalResult;

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
fn let(args: ValueRef, env: *const Env) anyerror!EvalResult {
    const pairs = _car(args);
    const expr = _cadr(args);
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
    const slice = toSlice(_car(args), &buf);

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
    var buf: [100]ValueRef = undefined;
    const cs = C.toArrayListUnmanaged(clauses, &buf);
    for (cs.items) |cl| {
        const pred = _car(cl);
        const p, e = try E.evaluate(pred, e);
        if (toBool(p)) return E.evaluate(_cadr(cl), e);
    }
    return .{ C.empty(), e }; // Return empty if all pred is false.
}

// special form
fn begin(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    var ret = C.empty();
    var new_env = env;

    var buf: [100]ValueRef = undefined;
    const xs = C.toArrayListUnmanaged(args, &buf);
    for (xs.items) |x| ret, new_env = try E.evaluate(x, new_env);
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
    var buf: [100]ValueRef = undefined;
    const nums = C.toArrayListUnmanaged(xs, &buf);
    for (nums.items) |x| ret += x.number;
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn sub(xs: ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    var buf: [100]ValueRef = undefined;
    const nums = C.toArrayListUnmanaged(xs, &buf);
    for (nums.items, 0..) |x, i| ret += if (i == 0) x.number else -x.number;
    return C.new(Value, Value{ .number = ret });
}

// built-in func
fn mul(xs: ValueRef) anyerror!ValueRef {
    var ret: i64 = 1;
    var buf: [100]ValueRef = undefined;
    const nums = C.toArrayListUnmanaged(xs, &buf);
    for (nums.items) |x| ret *= x.number;
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
    var buf: [100]ValueRef = undefined;
    const vals = C.toArrayListUnmanaged(xs, &buf);
    for (vals.items) |x| if (toBool(x)) return t();
    return f();
}

// built-in func
fn and_(xs: ValueRef) anyerror!ValueRef {
    var buf: [100]ValueRef = undefined;
    const vals = C.toArrayListUnmanaged(xs, &buf);
    for (vals.items) |x| if (!toBool(x)) return f();
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
    var buf: [100]ValueRef = undefined;
    const vals = C.toArrayListUnmanaged(xs, &buf);
    for (vals.items) |x| {
        const str = try C.toString(x);
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
    }
    return vals.getLastOrNull() orelse C.empty();
}

// special form
// (lambda (x y) (+ x y))
fn lambda(args: ValueRef, env: EnvRef) anyerror!EvalResult {
    const params = _car(args);
    const body = _cdr(args);
    var tmp: [100]ValueRef = undefined;
    const val_params = toSlice(params, &tmp);
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
fn defineSyntax(lst: ValueRef, env: EnvRef) anyerror!EvalResult {
    const name = _car(lst).symbol;
    const m = try new(Macro, Macro{
        .name = name,
        .rules = try parseRules(_cadr(lst)),
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
