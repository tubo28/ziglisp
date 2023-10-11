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

            if (cons.car.* == .symbol and
                if (env.get(cons.car.symbol)) |v| v.* == .macro else false)
            {
                const name = cons.car.symbol;
                std.log.err("macro `{s}`", .{S.getName(name).?});
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
    var rule: MacroRule = undefined;
    _ = rule;
    var matched = false;
    _ = matched;
    // for (macro.rules) |r| {
    //     if (try matchPattern(macro.name, r.pattern, expr)) {
    //         rule = r;
    //         matched = false;
    //     }
    // }
    // if (!matched) {
    //     std.log.err("expression doesn't match any rule of macro {s}", .{S.getName(macro.name).?});
    //     unreachable;
    // }

    return expr;
}

fn symbolp(x: ValueRef) ?SymbolID {
    switch (x.*) {
        Value.symbol => |s| return s,
        else => return null,
    }
}

// fn matches(name: []const u8, pat: []const u8, in: []const u8) !bool {
//     const P = @import("parse.zig");
//     const T = @import("tokenize.zig");
//     const p = (try P.parse(try T.tokenize(pat)))[0];
//     const i = (try P.parse(try T.tokenize(in)))[0];
//     return try matchPattern(try S.getOrRegister(name), p, i);
// }

// test "pattern matching" {
//     try S.init();
//     try std.testing.expect(try matches("macro", "(_ a b)", "(macro 0 1)"));
//     try std.testing.expect(!try matches("macro", "(_ a b)", "(bad 0 1)"));
//     try std.testing.expect(try matches("macro", "(_ (a (b c) d))", "(macro (0 (1 2) 3))"));
//     try std.testing.expect(!try matches("macro", "(_ (a (b c) d))", "(macro (0 1 2 3))"));
//     try std.testing.expect(try matches("macro", "(_ a ...)", "(macro 0)"));
//     try std.testing.expect(try matches("macro", "(_ a ...)", "(macro 0 1)"));
//     try std.testing.expect(try matches("macro", "(_ a ...)", "(macro 0 1 2)"));
//     try std.testing.expect(!try matches("macro", "(_ (a b) ...)", "(macro 0 1 2)"));
//     try std.testing.expect(try matches("macro", "(_ (a b) ...)", "(macro (0 1))"));
//     try std.testing.expect(try matches("macro", "(_ (a b) ...)", "(macro (0 1) (2 3))"));
// }

// const MatchResult = std.AutoHashMap(SymbolID, MatchTo);

// const MatchTo = union(enum) {
//     single: ValueRef,
//     variadic: std.ArrayList(ValueRef),
// };

// name:
//  if
// template:
// (define-syntax if
//   (syntax-rules ()
//     ((_ test then else)
//      (cond (test then)
//            (else else)))))
// epxr:
// (if a foo bar)
// fn matchPattern(name: SymbolID, pattern: ValueRef, input: ValueRef, result: *MatchResult, variadic: bool) !bool { // Want to return match detail
//     const self_name = try S.getOrRegister("_");
//     const dots = try S.getOrRegister("...");

//     switch (pattern.*) {
//         Value.number => |x| {
//             if (input.* != .number) return false;
//             return x == input.number;
//         },
//         Value.symbol => |s| {
//             if (variadic) {
//                 _, const val, const exists = try result.getOrPut(s);
//                 if (!exists) val.* = MatchTo{ .variadic = try std.ArrayList(ValueRef).init(C.alloc) };
//                 try val.variadic.append(input);
//             } else {
//                 _, const val, const exists = try result.getOrPut(s);
//                 if (!exists) {
//                     val.* = MatchTo{ .single = ValueRef };
//                 } else {
//                     std.log.err("macro parameter {s} is already bound to {any}", .{ S.getName(s).?, input });
//                     unreachable;
//                 }
//             }
//             return true;
//         },
//         Value.cons => |cons| {
//             if (input.* != .cons) return false;
//             if (symbolp(cons.car) == self_name) {
//                 if (symbolp(input.cons.car) != name) return false;
//                 return matchPattern(name, cons.cdr, input.cons.cdr);
//             }

//             const p_list = blk: {
//                 var tmp: [100]ValueRef = undefined;
//                 const l = C.toSlice(pattern, &tmp);
//                 break :blk tmp[0..l];
//             };
//             const i_list = blk: {
//                 var tmp: [100]ValueRef = undefined;
//                 const l = C.toSlice(input, &tmp);
//                 break :blk tmp[0..l];
//             };

//             const p_len = p_list.len;
//             const i_len = i_list.len;
//             if (p_list.len == 0) return i_len == 0;
//             if (p_list.len == 1) return matchPattern(name, p_list[0], i_list[0]);
//             if (p_list.len >= 2) {
//                 // Does template ends with '...'?
//                 if (p_list[p_len - 1].* == .symbol and p_list[p_len - 1].symbol == dots) {
//                     // If Input is too short, doesn't match.
//                     if (i_len < p_len - 1) return false;
//                     // Check leading elements matches.
//                     for (p_list[0 .. p_len - 1], i_list[0 .. p_len - 1]) |t, i|
//                         if (!try matchPattern(name, t, i)) return false;
//                     // Check trailing elements matches element just before '...'
//                     for (i_list[p_len - 1 .. i_len]) |i| {
//                         if (!try matchPattern(name, p_list[p_len - 2], i)) return false;
//                     }
//                     return true;
//                 } else {
//                     if (p_len != i_len) return false;
//                     for (p_list, i_list) |t, e|
//                         if (!try matchPattern(name, t, e)) return false;
//                     return true;
//                 }
//             }
//         },
//         else => {
//             std.log.err("invalid template", .{});
//             unreachable;
//         },
//     }
//     unreachable;
// }

const Callable = union(enum) {
    bsf: *const Builtin.SpecialForm,
    bfunc: *const Builtin.Function,
    func: *const Function,
};

fn toCallable(car: *const C.Value, env: EnvRef) !Callable {
    if (car.* == .cons) {
        // example: ((lambda (x) (+ x x)) 1)
        const lmd, _ = try evaluate(car, env);
        return Callable{ .func = lmd.function };
    }

    if (car.* == .symbol) {
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
