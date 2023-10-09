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

const Token = @import("tokenize.zig").Token;

const Builtin = @import("builtin.zig");

pub fn evaluate(x: ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.function, Value.b_func, Value.b_spf => unreachable,
        Value.symbol => |sym| return if (env.get(sym)) |ent| .{ ent, env } else .{ x, env },
        Value.cons => |cons| {
            if (x == C.empty()) {
                std.log.err("cannot evaluate empty list", .{});
                unreachable;
            }
            // Call something
            const c = try toCallable(cons.car, env);
            return call(c, cons.cdr, env);
        },
    }
}

const Callable = union(enum) {
    bsf: *const Builtin.SpecialForm,
    bfunc: *const Builtin.Function,
    func: *const Function,
};

fn toCallable(car: *const C.Value, env: EnvRef) !Callable {
    switch (car.*) {
        Value.cons => {
            // example: ((lambda (x) (+ x x)) 1)
            const lmd, _ = try evaluate(car, env);
            return Callable{ .func = lmd.function };
        },
        Value.symbol => |sym| {
            const func = env.get(sym);
            if (func == null) {
                std.log.err("symbol `{s}` is not callable", .{S.getName(sym).?});
                unreachable;
            }
            switch (func.?.*) {
                Value.function => |f_| return Callable{ .func = f_ },
                Value.b_func => |bf| return Callable{ .bfunc = Builtin.func[bf] },
                Value.b_spf => |bs| return Callable{ .bsf = Builtin.spf[bs] },
                else => |other| {
                    std.log.err("symbol `{s}` is bound to non-callable value: {any}", .{ S.getName(sym).?, other });
                    unreachable;
                },
            }
        },
        else => |other| {
            std.log.err("not callable: {}", .{other});
            unreachable;
        },
    }
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
    // if (func.name) |name| {
    //     // std.log.debug("{s}", .{S.getName(name).?});
    //     const t = try C.new(Value, Value{ .function = func });
    //     new_binds[i] = .{ name, t };
    //     i += 1;
    // }

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
