const std = @import("std");

const C = @import("common.zig");
const S = @import("symbol.zig");
const M = @import("map.zig");

const En = @import("env.zig");
const EnvRef = ValueRef;
const SymbolID = S.ID;
const Value = C.Value;
const ValueRef = C.ValueRef;

const Token = @import("tok.zig").Token;
const Builtin = @import("builtin.zig");
const newValue = @import("mem.zig").newValue;

pub const EvalResult = struct { ValueRef, EnvRef };

pub fn evaluate(x: ValueRef, env: EnvRef) anyerror!EvalResult {
    // std.log.debug("evaluate: {s}", .{try C.toString(x)});
    return if (x.* == .cons and C._car(x).* == .symbol and C._car(x).symbol == try S.getOrRegister("define"))
        define(C._cdr(x), env)
    else
        .{ try eval(x, env), env };
}

pub fn eval(x: ValueRef, env: EnvRef) anyerror!ValueRef {
    switch (x.*) {
        Value.number => return x,
        Value.lambda, Value.b_func, Value.b_form => unreachable,
        Value.symbol => return if (En.resolve(env, x)) |ent| ent else x,
        Value.cons => |cons| return call(cons.car, cons.cdr, env),
    }
}

fn call(car: *const C.Value, args: ValueRef, env: EnvRef) anyerror!ValueRef {
    if (car.* != .cons and car.* != .symbol) {
        std.log.err("not callable: {}", .{car.*});
        unreachable;
    }

    if (car.* == .cons) {
        // example: ((lambda (x) (+ x x)) 1)
        const lambda = try eval(car, env);
        return callLambda(lambda.lambda, args, env);
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

fn callForm(form: *const Builtin.SpecialForm, args: ValueRef, env: EnvRef) anyerror!ValueRef {
    return form(args, env);
}

fn callFunc(func: *const Builtin.Function, args: ValueRef, env: EnvRef) anyerror!ValueRef {
    return try func(try evalAll(args, env));
}

fn callLambda(lambda: ValueRef, args: ValueRef, env: EnvRef) anyerror!ValueRef {
    const params = C._car(lambda);
    const closure = C._cadr(lambda);
    const body = C._caddr(lambda);
    const lambda_env = switch (params.*) {
        // (lambda x (length x))
        Value.symbol => try M.addOne(closure, params, args),
        // (lambda (x . xs) (sum xs))
        else => try appendZipped(closure, params, args, env),
    };
    const ret = try eval(body, lambda_env);
    return ret;
}

fn appendZipped(append_to: ValueRef, names: ValueRef, values: ValueRef, env: EnvRef) !ValueRef {
    if (names == C.empty()) return append_to;

    switch (names.*) {
        Value.cons => |cons| {
            const to = try appendZipped(append_to, cons.cdr, values.cons.cdr, env);
            return try M.addOne(to, cons.car, try eval(values.cons.car, env));
        },
        Value.symbol => return try M.addOne(append_to, names, values),
        else => unreachable,
    }
}

fn evalAll(xs: ValueRef, env: EnvRef) anyerror!ValueRef {
    std.debug.assert(xs.* == .cons);
    if (xs == C.empty()) return xs;
    const car = try eval(xs.cons.car, env);
    const cdr = try evalAll(xs.cons.cdr, env);
    return try C.newCons(car, cdr);
}

// Define function or value
pub fn define(list: ValueRef, env: EnvRef) anyerror!EvalResult {
    std.debug.assert(C.listLength(list) > 0);
    switch (C._car(list).*) {
        Value.symbol => return defineValue(list, env),
        else => unreachable,
    }
}

// (define name body)
// defineValue is the only way to modify the global env.
fn defineValue(list: ValueRef, env: EnvRef) anyerror!EvalResult {
    std.debug.assert(C.listLength(list) == 2);
    const name = C._car(list);
    var bind_to: *Value = try newValue(undefined);
    try En.addGlobal(name, bind_to);
    const expr = C._cadr(list);
    const val = try eval(expr, env); // Assume that RHS has no side-effect.
    bind_to.* = val.*;
    return .{ val, env };
}
