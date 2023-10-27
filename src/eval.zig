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
        Value.cons => |cons| return call(cons.car, cons.cdr, env),
    }
}

fn call(car: *const C.Value, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    if (car.* != .cons and car.* != .symbol) {
        std.log.err("not callable: {}", .{car.*});
        unreachable;
    }

    if (car.* == .cons) {
        // example: ((lambda (x) (+ x x)) 1)
        const lmd, _ = try evaluate(car, env);
        return callLambda(lmd.lambda, args, env);
    }

    // car.* == .symbol
    const name = car;

    const callee = En.resolve(env, name);
    if (callee == null) {
        std.log.err("not callable value: {s} {}", .{ S.getName(name.symbol).?, car.* });
        unreachable;
    }
    switch (callee.?.*) {
        Value.lambda => |lambda| return callLambda(lambda, args, env),
        Value.b_func => |func| return callFunc(Builtin.func[func], args, env),
        Value.b_form => |form| return callForm(Builtin.form[form], args, env),
        else => |other| {
            std.log.err("symbol `{s}` is bound to non-callable value: {any}", .{ S.getName(name.symbol).?, other });
            unreachable;
        },
    }
}

fn callForm(form: *const Builtin.SpecialForm, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    return form(args, env);
}

fn callFunc(func: *const Builtin.Function, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    return .{ try func(try evalAll(args, env)), env };
}

fn callLambda(lambda: *const Lambda, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    var lambda_env = try M.addAll(lambda.closure, lambda.params, try evalAll(args, env));
    const ret, _ = try evaluate(lambda.body, lambda_env);
    return .{ ret, env };
}

fn evalAll(xs: ValueRef, env: EnvRef) anyerror!ValueRef {
    std.debug.assert(xs.* == .cons);
    if (xs == C.empty()) return xs;
    const car, _ = try evaluate(xs.cons.car, env);
    const cdr = try evalAll(xs.cons.cdr, env);
    return try C.newCons(car, cdr);
}
