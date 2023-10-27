const std = @import("std");

const C = @import("common.zig");
const S = @import("symbol.zig");
const M = @import("map.zig");

const En = @import("env.zig");
const EnvRef = ValueRef;
const EvalResult = C.EvalResult;
const Lambda = C.Lambda;
const SymbolID = S.ID;
const Value = C.Value;
const ValueRef = C.ValueRef;

const Token = @import("tok.zig").Token;

const Builtin = @import("builtin.zig");

pub fn evaluate(x: ValueRef, env: EnvRef) anyerror!EvalResult {
    // std.log.debug("evaluate: {s}", .{try C.toString(x)});
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.lambda, Value.b_func, Value.b_form => unreachable,
        Value.symbol => return if (En.resolve(env, x)) |ent| .{ ent, env } else .{ x, env },
        Value.cons => |cons| {
            // Call something
            const c = try toCallable(cons.car, env);
            return call(c, cons.cdr, env);
        },
    }
}

const Callable = union(enum) {
    b_form: *const Builtin.SpecialForm,
    b_func: *const Builtin.Function,
    lambda: *const Lambda,
};

fn toCallable(car: *const C.Value, env: EnvRef) !Callable {
    if (car.* != .cons and car.* != .symbol) {
        std.log.err("not callable: {}", .{car.*});
        unreachable;
    }

    if (car.* == .cons) {
        // example: ((lambda (x) (+ x x)) 1)
        const lmd, _ = try evaluate(car, env);
        return Callable{ .lambda = lmd.lambda };
    }

    const name = car;

    if (En.resolve(env, name) == null) {
        std.log.err("not callable value: {s} {}", .{ S.getName(name.symbol).?, car.* });
        unreachable;
    }

    const callable = En.resolve(env, name).?;

    switch (callable.*) {
        Value.lambda => |l| return Callable{ .lambda = l },
        Value.b_func => |bf| return Callable{ .b_func = Builtin.func[bf] },
        Value.b_form => |bs| return Callable{ .b_form = Builtin.form[bs] },
        else => |other| {
            std.log.err("symbol `{s}` is bound to non-callable value: {any}", .{ S.getName(name.symbol).?, other });
            unreachable;
        },
    }
}

fn call(callable: Callable, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (callable) {
        Callable.b_form => |form| return form(args, env),
        Callable.b_func => |func| return .{ try func(try evalAll(args, env)), env },
        Callable.lambda => |lambda| return .{ try callLambda(lambda, try evalAll(args, env)), env },
    }
}

fn evalAll(xs: ValueRef, env: EnvRef) !ValueRef {
    std.debug.assert(xs.* == .cons);
    if (xs == C.empty()) return xs;
    const car, _ = try evaluate(xs.cons.car, env);
    const cdr = try evalAll(xs.cons.cdr, env);
    return try C.newCons(car, cdr);
}

fn callLambda(lambda: *const Lambda, args: ValueRef) anyerror!ValueRef {
    var lambda_env = try M.addAll(lambda.closure, lambda.params, args);
    const ret, _ = try evaluate(lambda.body, lambda_env);
    return ret;
}
