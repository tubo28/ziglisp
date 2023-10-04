const std = @import("std");
const common = @import("common.zig");

const alloc = common.alloc;
const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;
const EvalResult = common.EvalResult;
const f = common.f;
const Function = common.Function;
const t = common.t;
const toString = common.toString;
const Value = common.Value;
const ValueRef = common.ValueRef;

const Symbol = @import("symbol.zig");
const SymbolID = Symbol.ID;

const Token = @import("tokenize.zig").Token;

const Builtin = @import("builtin.zig");

pub fn evaluate(x: ValueRef, env: EnvRef) anyerror!EvalResult {
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.function, Value.b_func, Value.b_spf => unreachable,
        Value.symbol => |sym| return if (env.get(sym)) |ent| .{ ent, env } else .{ x, env },
        Value.cons => |cons| {
            if (x == common.empty()) {
                std.log.err("cannot evaluate empty list", .{});
                unreachable;
            }
            // Call something
            const c = try toCallable(cons.car, env);
            const args = try common.toSlice(cons.cdr);
            return call(c, args, env);
        },
    }
}

const Callable = union(enum) {
    bsf: *const Builtin.SpecialForm,
    bfunc: *const Builtin.Function,
    func: *const Function,
};

fn toCallable(car: *const common.Value, env: EnvRef) !Callable {
    switch (car.*) {
        Value.cons => {
            // example: ((lambda (x) (+ x x)) 1)
            const lmd, _ = try evaluate(car, env);
            return Callable{ .func = lmd.function };
        },
        Value.symbol => |sym| {
            const func = env.get(sym);
            if (func == null) {
                std.log.err("symbol `{s}` is not callable", .{Symbol.getName(sym).?});
                unreachable;
            }
            switch (func.?.*) {
                Value.function => |f_| return Callable{ .func = f_ },
                Value.b_func => |bf| return Callable{ .bfunc = Builtin.func[bf] },
                Value.b_spf => |bs| return Callable{ .bsf = Builtin.spf[bs] },
                else => |other| {
                    std.log.err("symbol `{s}` is bound to non-callable value: {any}", .{ Symbol.getName(sym).?, other });
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
        const name = if (func.name) |n| Symbol.getName(n).? else "<lambda>";
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
        try new_binds.append(.{ name, try common.newFunctionValue(func) });

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
