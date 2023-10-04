const std = @import("std");

const C = @import("common.zig");
const S = @import("symbol.zig");

const alloc = C.alloc;
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
            const args = try C.toSlice(cons.cdr);
            return call(c, args, env);
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

fn call(callable: Callable, args: []ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (callable) {
        Callable.bsf => |form| return form(args, env),
        Callable.bfunc => |func| {
            const argSlice, const new_env = try evalAll(args, env);
            return .{ try func(argSlice), new_env };
        },
        Callable.func => |func| {
            const argSlice, const new_env = try evalAll(args, env);
            return .{ try callFunction(func, argSlice), new_env };
        },
    }
}

fn callFunction(func: *const Function, args: []ValueRef) anyerror!ValueRef {
    if (func.params.len != args.len) {
        const name = if (func.name) |n| S.getName(n).? else "<lambda>";
        std.log.err("wrong number of argument for {s}", .{name});
        unreachable;
    }

    var new_binds = std.ArrayList(struct { SymbolID, ValueRef }).init(alloc);
    defer new_binds.deinit();

    // Evaluate arguments.
    // Names and the function and arguments overrite function's namespace, what we call shadowing.
    for (func.params, args) |param, arg|
        try new_binds.append(.{ param, arg });
    if (func.name) |name|
        try new_binds.append(.{ name, try C.newFunctionValue(func) });

    var func_env = try func.env.overwrite(try new_binds.toOwnedSlice());

    // Eval body.
    std.debug.assert(func.body.len != 0);
    var ret: ValueRef = undefined;
    for (func.body) |expr| ret, func_env = try evaluate(expr, func_env);
    return ret;
}

fn evalAll(slice: []ValueRef, env: EnvRef) !struct { []ValueRef, EnvRef } {
    var ret = try alloc.alloc(ValueRef, slice.len);
    var e = env;
    for (ret, slice) |*x, y| x.*, e = try evaluate(y, e);
    return .{ ret, e };
}
