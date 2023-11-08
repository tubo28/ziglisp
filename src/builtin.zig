const std = @import("std");

const C = @import("common.zig");
const E = @import("eval.zig");
const S = @import("symbol.zig");
const M = @import("map.zig");
const Mem = @import("mem.zig");

const _car = C._car;
const _cdr = C._cdr;
const _cadr = C._cadr;
const _cddr = C._cddr;
const _caddr = C._caddr;
const f = C.f;
const newCons = C.newCons;
const t = C.t;
const Value = C.Value;
const ValueRef = C.ValueRef;

const newValue = Mem.newValue;

const En = @import("env.zig");
const EnvRef = ValueRef;

pub const Function = fn (ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn (ValueRef, EnvRef) anyerror!ValueRef;

const func_names = [_][]const u8{
    "car",
    "cdr",
    "cons",
    "list",
    "print",
    "+",
    "-",
    "*",
    "=",
    "<",
    "or",
    "and",
    "null?",
    "quotient",
    "modulo",
};
pub const func = [_]*const Function{
    Functions.car,
    Functions.cdr,
    Functions.cons_,
    Functions.list,
    Functions.print,
    Functions.add,
    Functions.sub,
    Functions.mul,
    Functions.eq,
    Functions.le,
    Functions.or_,
    Functions.and_,
    Functions.null_,
    Functions.quotient,
    Functions.modulo,
};

const form_names = [_][]const u8{
    "quote",
    "begin",
    "lambda",
    "if",
    "cond",
};
pub const form = [_]*const SpecialForm{
    SpecialForms.quote,
    SpecialForms.begin,
    SpecialForms.lambda,
    SpecialForms.if_,
    SpecialForms.cond,
};

pub fn loadBuiltin() !EnvRef {
    std.debug.assert(func_names.len == func.len);
    std.debug.assert(form_names.len == form.len);

    // Bind symbol and symbol id
    var names = C.empty();
    var values = C.empty();

    for (func_names, 100_000_000.., 0..) |name, sym_id, idx| {
        try S.registerUnsafe(name, @intCast(sym_id));
        names = try newCons(try newValue(Value{ .symbol = @intCast(sym_id) }), names);
        values = try newCons(try newValue(Value{ .b_func = idx }), values);
    }
    for (form_names, 200_000_000.., 0..) |name, sym_id, idx| {
        try S.registerUnsafe(name, @intCast(sym_id));
        names = try newCons(try newValue(Value{ .symbol = @intCast(sym_id) }), names);
        values = try newCons(try newValue(Value{ .b_form = idx }), values);
    }

    return try M.addAll(C.empty(), names, values);
}

// lambda and define
const SpecialForms = struct {
    fn quote(list: ValueRef, env: EnvRef) anyerror!ValueRef {
        _ = env;
        return _car(list);
    }

    fn if_(list: ValueRef, env: EnvRef) anyerror!ValueRef {
        const pred = _car(list);
        const then = _cadr(list);
        const p = try E.eval(pred, env);
        if (toBool(p)) return try E.eval(then, env);
        const unless = if (C.listLength(list) >= 3) _caddr(list) else null;
        if (unless) |u| return try E.eval(u, env);
        return C.empty(); // Return empty if pred is false and unless is not given.
    }

    fn cond(clauses: ValueRef, env: EnvRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(clauses, &buf);
        for (slice.items) |cl| {
            const pred = _car(cl);
            const p = try E.eval(pred, env);
            if (toBool(p)) return E.eval(_cadr(cl), env);
        }
        return C.empty(); // Return empty if all pred is false.
    }

    fn begin(exprs: ValueRef, env: EnvRef) anyerror!ValueRef {
        var ret = C.empty();
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(exprs, &buf);
        for (slice.items) |expr| ret = try E.eval(expr, env);
        return ret; // return the last result
    }

    // (lambda (x y) (+ x y))
    // Lambda captures outer env (lexical scope).
    fn lambda(list: ValueRef, env: EnvRef) anyerror!ValueRef {
        std.debug.assert(C.listLength(list) >= 2);
        const params = _car(list);
        const body = _cadr(list);

        const l = try C.newCons(params, try C.newCons(env, try C.newCons(body, C.empty())));
        const func_val = try newValue(Value{ .lambda = l });
        return func_val;
    }
};

const Functions = struct {
    fn car(xs: ValueRef) anyerror!ValueRef {
        std.debug.assert(C.listLength(xs) == 1);
        return _car(_car(xs));
    }

    fn cdr(xs: ValueRef) anyerror!ValueRef {
        std.debug.assert(C.listLength(xs) == 1);
        return _cdr(_car(xs));
    }

    fn cons_(xs: ValueRef) anyerror!ValueRef {
        std.debug.assert(C.listLength(xs) == 2);
        return C.newCons(_car(xs), _cadr(xs));
    }

    fn list(xs: ValueRef) anyerror!ValueRef {
        return xs;
    }

    fn add(xs: ValueRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(xs, &buf);
        var ret: i64 = 0;
        for (slice.items) |x| ret += x.number;
        return newValue(Value{ .number = ret });
    }

    fn sub(xs: ValueRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(xs, &buf);
        var ret: i64 = 0;
        for (slice.items, 0..) |x, i| ret += if (i == 0) x.number else -x.number;
        return newValue(Value{ .number = ret });
    }

    fn mul(xs: ValueRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(xs, &buf);
        var ret: i64 = 1;
        for (slice.items) |x| ret *= x.number;
        return newValue(Value{ .number = ret });
    }

    fn quotient(xs: ValueRef) anyerror!ValueRef {
        const ret = @divFloor(_car(xs).number, _cadr(xs).number);
        return newValue(Value{ .number = ret });
    }

    fn modulo(xs: ValueRef) anyerror!ValueRef {
        const ret = @mod(_car(xs).number, _cadr(xs).number);
        return newValue(Value{ .number = ret });
    }

    fn or_(xs: ValueRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(xs, &buf);
        for (slice.items) |x| if (toBool(x)) return t();
        return f();
    }

    fn and_(xs: ValueRef) anyerror!ValueRef {
        var buf: [100]ValueRef = undefined;
        const slice = C.flattenToALU(xs, &buf);
        for (slice.items) |x| if (!toBool(x)) return f();
        return t();
    }

    fn eq(xs: ValueRef) anyerror!ValueRef {
        if (C.deepEql(_car(xs), _cadr(xs))) return t();
        return f();
    }

    fn le(xs: ValueRef) anyerror!ValueRef {
        return toValue(_car(xs).number < _cadr(xs).number);
    }

    fn null_(xs: ValueRef) anyerror!ValueRef {
        return toValue(_car(xs) == C.empty());
    }

    fn print(xs: ValueRef) !ValueRef {
        if (xs == C.empty()) return C.empty();
        const str = try C.toString(_car(xs));
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
        if (_cdr(xs) == C.empty()) return _car(xs);
        return print(_cdr(xs));
    }
};

fn toValue(x: bool) anyerror!ValueRef {
    return if (x) t() else f();
}

fn toBool(x: ValueRef) bool {
    return !isF(x);
}

fn isF(x: ValueRef) bool {
    return x == f();
}
