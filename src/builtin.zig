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

const form_names = [_][]const u8{
    "quote",
    "begin",
    "define",
    "lambda",
    "if",
    "cond",
    "define-syntax",
};
pub const form = [_]*const SpecialForm{
    SpecialForms.quote,
    SpecialForms.begin,
    SpecialForms.define,
    SpecialForms.lambda,
    SpecialForms.if_,
    SpecialForms.cond,
    SpecialForms.defineSyntax,
};

pub fn loadBuiltin() !EnvRef {
    std.debug.assert(func_names.len == func.len);
    std.debug.assert(form_names.len == form.len);

    // Bind symbol and symbol id
    var names = try C.alloc.alloc(S.ID, 100);
    var values = try C.alloc.alloc(ValueRef, 100);

    var i: usize = 0;
    for (func_names, 100_000_000.., 0..) |name, sym_id, idx| {
        try S.registerUnsafe(name, @intCast(sym_id));
        names[i] = @intCast(sym_id);
        values[i] = try new(Value, Value{ .b_func = idx });
        i += 1;
    }
    for (form_names, 200_000_000.., 0..) |name, sym_id, idx| {
        try S.registerUnsafe(name, @intCast(sym_id));
        names[i] = @intCast(sym_id);
        values[i] = try new(Value, Value{ .b_form = idx });
        i += 1;
    }

    return try Env.new(names, values, i);
}

// lambda and define
const SpecialForms = struct {
    fn quote(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        return .{ args[0], env };
    }

    // Define function or value
    fn define(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len > 0);
        switch (args[0].*) {
            Value.symbol => return defineV(args, env),
            else => unreachable,
        }
    }

    // (define name body)
    // defineV is the only way to modify the global env.
    fn defineV(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len == 2);
        const name = args[0].symbol;
        var bind_to: *Value = try C.new(Value, undefined);
        env.globalDef(name, bind_to);
        const expr = args[1];
        const val, _ = try E.evaluate(expr, env); // Assume that RHS has no side-effect.
        bind_to.* = val.*;
        return .{ val, env };
    }

    fn if_(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        const pred = args[0];
        const then = args[1];
        const unless = if (args.len >= 3) args[2] else null;
        const p, _ = try E.evaluate(pred, env);
        if (toBool(p)) return try E.evaluate(then, env);
        if (unless) |u| return try E.evaluate(u, env);
        return .{ C.empty(), env }; // Return empty if pred is false and unless is not given.
    }

    fn cond(clauses: []ValueRef, env: EnvRef) anyerror!EvalResult {
        for (clauses) |cl| {
            const pred = _car(cl);
            const p, _ = try E.evaluate(pred, env);
            if (toBool(p)) return E.evaluate(_cadr(cl), env);
        }
        return .{ C.empty(), env }; // Return empty if all pred is false.
    }

    fn begin(exprs: []ValueRef, env: EnvRef) anyerror!EvalResult {
        var ret = C.empty();
        for (exprs) |expr| ret, _ = try E.evaluate(expr, env);
        return .{ ret, env }; // return the last result
    }

    // (lambda (x y) (+ x y))
    // Lambda captures outer env (lexical scope).
    fn lambda(args: []ValueRef, env: EnvRef) anyerror!EvalResult {
        std.debug.assert(args.len >= 2);
        const params = args[0];
        const body = args[1];
        // TODO: change params in body to lval
        // Convert ValueRefs to symbols
        var sym_params = b: {
            var tmp: [100]ValueRef = undefined;
            const val_params = C.toArrayListUnmanaged(params, &tmp);
            var ret = try alloc.alloc(S.ID, val_params.items.len);
            for (val_params.items, 0..) |a, i| ret[i] = a.symbol;
            break :b ret;
        };
        const func_val = try C.new(Value, Value{
            .lambda = try new(C.Lambda, C.Lambda{
                .params = sym_params,
                .body = body,
                .closure = env, // capture env
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
        env.globalDef(name, macro);
        return .{ macro, env };
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
