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
                std.log.err("empty list cannot be evaluated", .{});
                unreachable;
            }
            switch (cons.car.*) {
                Value.number => @panic("number cannot be a function"),
                Value.cons => {
                    // Code like ((lambda (x) (+ x x)) 1) goes through here
                    const l, var new_env = try evaluate(cons.car, env);
                    return call(l.function, cons.cdr, new_env);
                },
                Value.function, Value.b_func, Value.b_spf => @panic("unimplemented"),
                Value.symbol => |sym| {
                    if (env.get(sym)) |func| {
                        return switch (func.*) {
                            Value.function => |f_| call(f_, cons.cdr, env),
                            Value.b_func => |bf| call(Builtin.func[bf], cons.cdr, env),
                            Value.b_spf => |bs| call(Builtin.spf[bs], cons.cdr, env),
                            else => unreachable,
                        };
                    }
                    // switch (func.*) {
                    //     Value.function => |ff| return .{ try callFunction(ff, args), env },
                    //     // I can't remember why I wrote this code
                    //     // Value.cons => {
                    //     //     const l, _ = try evaluate(func, new_env);
                    //     //     return .{ try callFunction(l, args), env };
                    //     // },
                    //     else => @panic("symbol not binded to function"),
                    // }
                    std.log.err("function or special form not defined: {s} ({})\n", .{ Symbol.getName(sym).?, sym });
                    unreachable;
                },
            }
        },
    }
}

fn call(val: anytype, args: ValueRef, env: EnvRef) anyerror!EvalResult {
    const ty = @TypeOf(val);
    switch (ty) {
        *const Builtin.SpecialForm => return val(try common.toSlice(args), env),
        *const Builtin.Function => {
            const argSlice, const new_env = try toSliceE(args, env);
            return .{ try val(argSlice), new_env };
        },
        *const Function => { // lambda or define
            const argSlice, const new_env = try toSliceE(args, env);
            return .{ try callFunction(val, argSlice), new_env };
        },
        else => {
            std.log.err("not callable value: {}", .{val});
            std.log.err("ty: {}", .{ty});
            unreachable;
        },
    }
}

var args_and_self_sid: [128]SymbolID = undefined;
var args_and_self: [128]ValueRef = undefined;

fn callFunction(func: *const Function, args: []ValueRef) anyerror!ValueRef {
    //    const func = x.function;
    if (func.params.len != args.len) {
        const name = if (func.name) |n| Symbol.getName(n).? else "<lambda>";
        std.log.err("wrong number of argument for {s}", .{name});
        @panic("wrong number of arguments for function");
    }

    // Evaluate arguments.
    // Names and the function and arguments overrite function's namespace, what we call 'shadowing'.

    for (func.params, args, 0..) |param, arg, i| {
        args_and_self_sid[i] = param;
        args_and_self[i] = arg;
    }
    var len = func.params.len;
    if (func.name) |name| {
        args_and_self_sid[len] = name;
        args_and_self[len] = try common.newFunctionValue(func);
        len += 1;
    }

    var func_env = try func.env.overwrite(args_and_self_sid[0..len], args_and_self[0..len]);

    // Eval body.
    std.debug.assert(func.body.len != 0);
    var ret: ValueRef = undefined;
    for (func.body) |expr| ret, func_env = try evaluate(expr, func_env);
    return ret;
}

fn toSliceE(head: ValueRef, env: EnvRef) !struct { []ValueRef, EnvRef } {
    var ret = try common.toSlice(head);
    var e = env;
    for (ret) |*x| x.*, e = try evaluate(x.*, e);
    return .{ ret, e };
}
