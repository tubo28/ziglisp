const std = @import("std");

const C = @import("common.zig");
const S = @import("symbol.zig");
const M = @import("macro.zig");

const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;
const EvalResult = C.EvalResult;
const Function = C.Function;
const SymbolID = S.ID;
const Value = C.Value;
const ValueRef = C.ValueRef;
const Macro = C.Macro;
const MacroRule = C.MacroRule;

const Token = @import("tokenize.zig").Token;

const Builtin = @import("builtin.zig");

pub fn evaluate(x: ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.function, Value.b_func, Value.b_spf, Value.macro => unreachable,
        Value.symbol => |sym| return if (env.get(sym)) |ent| .{ ent, env } else .{ x, env },
        Value.cons => |cons| {
            if (x == C.empty()) {
                std.log.err("cannot evaluate empty list", .{});
                unreachable;
            }

            if (@as(C.ValueTag, cons.car.*) == C.ValueTag.symbol and
                if (env.get(cons.car.symbol)) |v| @as(C.ValueTag, v.*) == C.ValueTag.macro else false)
            {
                const name = cons.car.symbol;
                std.log.err("macro {s}!", .{S.getName(name).?});
                const macro = env.get(name).?.macro;
                const expr = try expandMacro(cons.cdr, macro);
                return evaluate(expr, env);
            }

            // Call something
            const c = try toCallable(cons.car, env);
            return call(c, cons.cdr, env);
        },
    }
}

// expr -> expr
fn expandMacro(expr: ValueRef, macro: *const Macro) !ValueRef {
    _ = macro;
    _ = expr;
    unreachable;
}

fn matchRule(macro: *const Macro, template: ValueRef, expr: ValueRef) bool {
    const us = S.getName("_").?;

    if (@as(C.ValueTag, template) != @as(C.ValueTag, expr.*)) return false;
    switch (template.*) {
        Value.symbol => return true,
        Value.cons => |cons| {
            if (cons.car == us) return expr.cons.car == macro.name;

            return matchRule(cons.car, expr.cons.car) and matchRule(cons.cdr, expr.cons.cdr);
        },
    }
    unreachable;
}

const Callable = union(enum) {
    bsf: *const Builtin.SpecialForm,
    bfunc: *const Builtin.Function,
    func: *const Function,
};

fn toCallable(car: *const C.Value, env: EnvRef) !Callable {
    if (@as(C.ValueTag, car.*) == C.ValueTag.cons) {
        // example: ((lambda (x) (+ x x)) 1)
        const lmd, _ = try evaluate(car, env);
        return Callable{ .func = lmd.function };
    }

    if (@as(C.ValueTag, car.*) == C.ValueTag.symbol) {
        const sym = car.symbol;
        const callable = env.get(sym);
        if (callable == null) {
            std.log.err("symbol `{s}` is not callable", .{S.getName(sym).?});
            unreachable;
        }
        switch (callable.?.*) {
            Value.function => |f_| return Callable{ .func = f_ },
            Value.b_func => |bf| return Callable{ .bfunc = Builtin.func[bf] },
            Value.b_spf => |bs| return Callable{ .bsf = Builtin.spf[bs] },
            else => |other| {
                std.log.err("symbol `{s}` is bound to non-callable value: {any}", .{ S.getName(sym).?, other });
                unreachable;
            },
        }
    }

    std.log.err("not callable: {}", .{car.*});
    unreachable;
}

fn call(callable: Callable, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (callable) {
        Callable.bsf => |form| return form(args, env),
        Callable.bfunc => |func| {
            const argSlice, const new_env = try evalList(args, env);
            return .{ try func(argSlice), new_env };
        },
        Callable.func => |func| {
            const argSlice, const new_env = try evalList(args, env);
            return .{ try callFunction(func, argSlice), new_env };
        },
    }
}

fn callFunction(func: *const Function, args: ValueRef) anyerror!ValueRef {
    var new_binds: [100]struct { SymbolID, ValueRef } = undefined;
    var i: usize = 0;

    // Evaluate arguments.
    // Names and the function and arguments overrite function's namespace, what we call shadowing.
    {
        var h = args;
        while (h != C.empty()) {
            const arg = h.cons.car;
            new_binds[i] = .{ func.params[i], arg };
            i += 1;
            h = h.cons.cdr;
        }
    }
    std.debug.assert(i == func.params.len);

    var func_env = try func.env.overwrite(new_binds[0..i]);

    // Eval body.
    std.debug.assert(func.body != C.empty());
    var ret: ValueRef = undefined;
    {
        var h = func.body;
        while (h != C.empty()) {
            const expr = h.cons.car;
            ret, func_env = try evaluate(expr, func_env);
            h = h.cons.cdr;
        }
    }
    return ret;
}

fn evalList(list: ValueRef, env: EnvRef) !struct { ValueRef, EnvRef } {
    var e = env;

    var h = list;
    var slice: [100]ValueRef = undefined;
    var i: usize = 0;
    while (h != C.empty()) {
        const x, e = try evaluate(h.cons.car, e);
        slice[i] = x;
        i += 1;
        h = h.cons.cdr;
    }
    var ret = C.empty();
    while (i != 0) {
        i -= 1;
        ret = try C.newCons(slice[i], ret);
    }

    return .{ ret, e };
}

// pub fn preproc(exprs: []ValueRef) ![]ValueRef {
//     var ret = std.ArrayList(ValueRef).init(C.alloc);

//     var macros = std.AutoHashMap(S.ID, Macro).init(C.alloc);
//     // Ignores `define-syntax` on non top level position.
//     for (exprs) |expr| {
//         if (@as(C.ValueTag, expr.*) == C.ValueTag.cons and
//             @as(C.ValueTag, expr.cons.car.*) == C.ValueTag.symbol and
//             expr.cons.car.symbol == try S.getOrRegister("define-syntax"))
//         {
//             const macro = Macro{
//                 .name = C._cadr(expr).symbol,
//                 .rules = try C.parseRules(C._caddr(expr)),
//             };
//             try macros.put(macro.name, macro);
//             continue;
//         }

//         try ret.append(expr);
//     }
//     return ret.toOwnedSlice();
// }
