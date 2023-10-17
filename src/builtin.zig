const std = @import("std");

const C = @import("common.zig");
const E = @import("evaluate.zig");
const S = @import("symbol.zig");

const _cadr = C._cadr;
const _car = C._car;
const _cdr = C._cdr;
const _cddr = C._cddr;
const alloc = C.alloc;
const EvalResult = C.EvalResult;
const f = C.f;
const new = C.new;
const t = C.t;
const Value = C.Value;
const ValueRef = C.ValueRef;
const Macro = C.Macro;
const MacroRule = C.MacroRule;

const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;

pub const Function = fn ([]ValueRef) anyerror!ValueRef;
pub const SpecialForm = fn ([]ValueRef, EnvRef) anyerror!EvalResult;

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

const spf_names = [_][]const u8{
    "quote",
    "begin",
    "define",
    "lambda",
    "if",
    "cond",
    "let",
    "define-syntax",
};
pub const spf = [_]*const SpecialForm{
    SpecialForms.quote,
    SpecialForms.begin,
    SpecialForms.define,
    SpecialForms.lambda,
    SpecialForms.if_,
    SpecialForms.cond,
    SpecialForms.let,
    SpecialForms.defineSyntax,
};

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

const SpecialForms = struct {
    fn let(args: []ValueRef, env: *const Env) anyerror!EvalResult {
        std.debug.assert(args.len == 2);
        const pairs = args[0];
        var buf: [100]ValueRef = undefined;
        const pairsSlice = C.toArrayListUnmanaged(pairs, &buf);

        var new_binds = std.ArrayList(struct { S.ID, ValueRef }).init(alloc);
        defer new_binds.deinit();

        for (pairsSlice.items) |p| {
            const k = _car(p).symbol;
            const v = _cadr(p);
            // No dependency between new values.
            const ev, _ = try E.evaluate(v, env);
            try new_binds.append(.{ k, ev });
        }

        const new_env = try env.overwrite(try new_binds.toOwnedSlice());
        const expr = args[1];
        return E.evaluate(expr, new_env);
    }

    fn quote(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        return .{ args[0], env };
    }

    // Define function or value
    fn define(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len > 0);
        switch (args[0].*) {
            Value.cons => return defineF(args, env),
            Value.symbol => return defineV(args, env),
            else => unreachable,
        }
    }

    // (define (name params) body ...+)
    fn defineF(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        // Convert
        // (define (name param*) body+)
        // to
        // (define name (lambda (param*) body+))
        const a = try C.toConsList(args);
        const name = _car(_car(a));
        const params = _cdr(_car(a));
        const body = _cdr(a);

        var lambda_params: [2]ValueRef = undefined;
        lambda_params[0] = name;
        lambda_params[1] = try C.newCons(
            try new(Value, Value{ .symbol = try S.getOrRegister("lambda") }),
            try C.newCons(params, body),
        );
        return defineV(&lambda_params, env);
    }

    // (define name body)
    fn defineV(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len == 2);
        const name = args[0].symbol;
        var bind_to: *Value = try C.new(Value, undefined);
        const new_env = try env.overwriteOne(name, bind_to);
        const expr = args[1];
        const val, _ = try E.evaluate(expr, new_env); // Assume that RHS has no side-effect.
        bind_to.* = val.*;
        return .{ val, new_env };
    }

    fn if_(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        const pred = args[0];
        const then = args[1];
        const unless = if (args.len >= 3) args[2] else null;
        const p, const new_env = try E.evaluate(pred, env);
        if (toBool(p)) return try E.evaluate(then, new_env);
        if (unless) |u| return try E.evaluate(u, new_env);
        return .{ C.empty(), new_env }; // Return empty if pred is false and unless is not given.
    }

    fn cond(clauses: []ValueRef, env: EnvRef) anyerror!EvalResult {
        var e = env;
        for (clauses) |cl| {
            const pred = _car(cl);
            const p, e = try E.evaluate(pred, e);
            if (toBool(p)) return E.evaluate(_cadr(cl), e);
        }
        return .{ C.empty(), e }; // Return empty if all pred is false.
    }

    fn begin(exprs: []ValueRef, env: EnvRef) anyerror!EvalResult {
        var ret = C.empty();
        var new_env = env;
        for (exprs) |expr| ret, new_env = try E.evaluate(expr, new_env);
        return .{ ret, new_env }; // return the last result
    }

    // (lambda (x y) (+ x y))
    fn lambda(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len >= 2);
        const params = args[0];
        const body = try alloc.alloc(ValueRef, args.len - 1);
        std.mem.copy(ValueRef, body, args[1..]);
        // Convert ValueRefs to symbols
        var sym_params = b: {
            var tmp: [100]ValueRef = undefined;
            const val_params = C.toArrayListUnmanaged(params, &tmp);
            var ret = try alloc.alloc(S.ID, val_params.items.len);
            for (val_params.items, 0..) |a, i| ret[i] = a.symbol;
            break :b ret;
        };
        const func_val = try C.new(Value, Value{
            .function = try new(C.Function, C.Function{
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
};

const Functions = struct {
    fn car(args: []ValueRef) anyerror!ValueRef {
        return _car(args[0]);
    }

    fn cdr(args: []ValueRef) anyerror!ValueRef {
        return _cdr(args[0]);
    }

    fn cons_(args: []ValueRef) anyerror!ValueRef {
        return C.newCons(args[0], args[1]);
    }

    fn list(xs: []ValueRef) anyerror!ValueRef {
        if (xs.len == 0) return C.empty();
        return C.newCons(xs[0], try list(xs[1..]));
    }

    fn add(xs: []ValueRef) anyerror!ValueRef {
        var ret: i64 = 0;
        for (xs) |x| ret += x.number;
        return C.new(Value, Value{ .number = ret });
    }

    fn sub(xs: []ValueRef) anyerror!ValueRef {
        var ret: i64 = 0;
        for (xs, 0..) |x, i| ret += if (i == 0) x.number else -x.number;
        return C.new(Value, Value{ .number = ret });
    }

    fn mul(xs: []ValueRef) anyerror!ValueRef {
        var ret: i64 = 1;
        for (xs) |x| ret *= x.number;
        return C.new(Value, Value{ .number = ret });
    }

    fn quotient(xs: []ValueRef) anyerror!ValueRef {
        const ret = @divFloor(xs[0].number, xs[1].number);
        return C.new(Value, Value{ .number = ret });
    }

    fn modulo(xs: []ValueRef) anyerror!ValueRef {
        const ret = @mod(xs[0].number, xs[1].number);
        return C.new(Value, Value{ .number = ret });
    }

    fn or_(xs: []ValueRef) anyerror!ValueRef {
        for (xs) |x| if (toBool(x)) return t();
        return f();
    }

    fn and_(xs: []ValueRef) anyerror!ValueRef {
        for (xs) |x| if (!toBool(x)) return f();
        return t();
    }

    fn eq(xs: []ValueRef) anyerror!ValueRef {
        if (C.deepEql(xs[0], xs[1])) return t();
        return f();
    }

    fn le(xs: []ValueRef) anyerror!ValueRef {
        return toValue(xs[0].number < xs[1].number);
    }

    fn null_(xs: []ValueRef) anyerror!ValueRef {
        return toValue(xs[0] == C.empty());
    }

    fn print(xs: []ValueRef) !ValueRef {
        if (xs.len == 0) return C.empty();
        for (xs) |x| {
            const str = try C.toString(x);
            const stdout = std.io.getStdOut().writer();
            nosuspend try stdout.print("#print: {s}\n", .{str});
        }
        return xs[xs.len - 1];
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

fn parseRules(lst: ValueRef) ![]MacroRule {
    const syntax_rules = try S.getOrRegister("syntax-rules");

    std.debug.assert(_car(lst).symbol == syntax_rules);
    std.debug.assert(_cadr(lst) == C.empty()); // TODO

    var tmp: [100]ValueRef = undefined;
    const rules = C.toArrayListUnmanaged(_cddr(lst), &tmp);

    var ret = try alloc.alloc(MacroRule, rules.items.len);
    for (rules.items, 0..) |r, i|
        ret[i] = MacroRule{ .pattern = _car(r), .template = _cadr(r) };
    return ret;
}
