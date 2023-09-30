const std = @import("std");

const common = @import("common.zig");
const alloc = common.alloc;
const Function = common.Function;
const Map = common.Map;
const t = common.t;
const f = common.f;
const toString = common.toString;
const ValueRef = common.ValueRef;
const Value = common.Value;

const Symbol = @import("symbol.zig");
const SymbolID = Symbol.ID;

const Token = @import("tokenize.zig").Token;

pub const EvalResult = struct { ValueRef, Map };

pub fn evaluate(x: ValueRef, env: Map) anyerror!EvalResult {
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.function => unreachable,
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
                Value.function => @panic("unimplemented"),
                Value.symbol => |sym| {
                    // User-defined functions.
                    if (env.get(sym)) |func| return call(func.function, cons.cdr, env);
                    // switch (func.*) {
                    //     Value.function => |ff| return .{ try callFunction(ff, args), env },
                    //     // I can't remember why I wrote this code
                    //     // Value.cons => {
                    //     //     const l, _ = try evaluate(func, new_env);
                    //     //     return .{ try callFunction(l, args), env };
                    //     // },
                    //     else => @panic("symbol not binded to function"),
                    // }
                    if (special_form(sym)) |func| return call(func, cons.cdr, env);
                    if (builtin_func(sym)) |func| return call(func, cons.cdr, env);
                    std.log.err("function or special form not defined: {s} ({})\n", .{ Symbol.getName(sym).?, sym });
                    unreachable;
                },
            }
        },
    }
}

fn call(val: anytype, args: ValueRef, env: Map) anyerror!EvalResult {
    const ty = @TypeOf(val);
    switch (ty) {
        *const SpecialForm => return val(try toSlice(args), env),
        *const BuiltinFunc => {
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

fn callFunction(func: *const Function, args: []ValueRef) anyerror!ValueRef {
    //    const func = x.function;
    if (func.params.len != args.len) {
        const name = if (func.name) |n| Symbol.getName(n).? else "<lambda>";
        std.log.err("wrong number of argument for {s}", .{name});
        @panic("wrong number of arguments for function");
    }

    var new_env = try func.env.clone();
    // Evaluate arguments.
    // Overwrite env entries if exist.
    // It means argument name shadows the values with same name at the defined time.
    // For function name
    if (func.name) |name| try new_env.put(name, try common.newFunctionValue(func));
    // For arguments
    for (func.params, args) |param, arg|
        try new_env.put(param, arg);

    // Eval body.
    std.debug.assert(func.body.len != 0);
    var ret: ValueRef = undefined;
    for (func.body) |expr| ret, new_env = try evaluate(expr, new_env);
    return ret;
}

// Convert sequence of cons cell like (foo bar buz) to slice.
fn toSlice(head: ValueRef) ![]ValueRef {
    std.debug.assert(atomp(head) == null); // is cons?

    var ret = std.ArrayList(ValueRef).init(alloc); // defer deinit?
    var h = head;
    while (h != common.empty()) {
        // TODO: Check that h is cons
        const x = h.cons.car;
        ret.append(x) catch @panic("cannot append");
        h = h.cons.cdr;
    }
    return try ret.toOwnedSlice();
}

fn toSliceE(head: ValueRef, env: Map) !struct { []ValueRef, Map } {
    var ret = try toSlice(head);
    var e = try env.clone();
    for (ret) |*x| x.*, e = try evaluate(x.*, env);
    return .{ ret, e };
}

pub fn init() !void {
    try Symbol.registerMany(special_form_names, special_form_mask);
    try Symbol.registerMany(builtin_func_names, builtin_func_mask);
}

const special_form_mask: SymbolID = 1 << 30;
const SpecialForm = fn ([]ValueRef, Map) anyerror!EvalResult;
const special_form_names = [_][]const u8{ "quote", "begin", "define", "lambda", "if", "cond", "let" };
const special_form_funcs = [_]*const SpecialForm{ quote, begin, defineFunction, lambda, if_, cond, let };
fn special_form(sid: SymbolID) ?*const SpecialForm {
    return if (sid & special_form_mask != 0) special_form_funcs[sid ^ special_form_mask] else null;
}

const BuiltinFunc = fn ([]ValueRef) anyerror!ValueRef;
fn builtin_func(sid: SymbolID) ?*const BuiltinFunc {
    return if (sid & builtin_func_mask != 0) builtin_func_funcs[sid ^ builtin_func_mask] else null;
}
const builtin_func_mask: SymbolID = 1 << 29;
const builtin_func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "or", "and", "length", "null?", "quotient", "modulo" };
// TODO: Move some of list items to another file and read it with @embedFile
const builtin_func_funcs = [_]*const BuiltinFunc{ car, cdr, cons_, list, print, add, sub, mul, eq, le, or_, and_, length, null_, quotient, modulo };

// special form
fn quote(args: []ValueRef, env: Map) anyerror!EvalResult {
    return .{ args[0], env };
}

// special form
fn if_(args: []ValueRef, env: Map) anyerror!EvalResult {
    const pred = args[0];
    const then = args[1];
    const unless = if (args.len >= 3) args[2] else null;
    const p, const new_env = try evaluate(pred, env);
    if (toBool(p)) return try evaluate(then, new_env);
    if (unless) |u| return try evaluate(u, new_env);
    return .{ common.empty(), new_env }; // Return empty if pred is false and unless is not given.
}

// special form
fn cond(clauses: []ValueRef, env: Map) anyerror!EvalResult {
    var e = try env.clone();
    for (clauses) |c| {
        const tmp = try toSlice(c);
        const pred = tmp[0];
        const then = tmp[1];
        const p, e = try evaluate(pred, e);
        if (toBool(p)) return evaluate(then, e);
    }
    return .{ common.empty(), e }; // Return empty if all pred is false.
}

fn toBool(x: ValueRef) bool {
    return !isF(x);
}

fn isF(x: ValueRef) bool {
    return x == common.f();
}

// special form
fn let(args: []ValueRef, env: Map) anyerror!EvalResult {
    const pairs = args[0];
    const expr = args[1];
    const pairsSlice = try toSlice(pairs);
    const n = pairsSlice.len;

    var keys: []Symbol.ID = try alloc.alloc(Symbol.ID, n);
    defer alloc.free(keys);
    var vals: []ValueRef = try alloc.alloc(ValueRef, n);
    defer alloc.free(vals);

    for (pairsSlice, 0..) |p, i| {
        const keyVal = try toSlice(p);
        keys[i] = keyVal[0].symbol;
        vals[i] = keyVal[1];
    }

    // The prior binding is not used to evaluate the following binding,
    // but the result of evaluating the RHS of a prior ones are propagated.
    var new_env = try env.clone();
    for (0..vals.len) |i|
        vals[i], new_env = try evaluate(vals[i], new_env);
    for (keys, vals) |k, v| new_env = try putPure(new_env, k, v);
    return evaluate(expr, new_env);
}

// special form
// (define (head args) body ...+)
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
fn defineFunction(args: []ValueRef, env: Map) anyerror!EvalResult {
    const params = args[0];
    const body = args[1..];
    std.debug.assert(body.len != 0); // Ill-formed special form
    const slice = try toSlice(params);
    const name = slice[0];

    var sym_params = std.ArrayList(Symbol.ID).init(alloc);
    for (slice[1..]) |arg| try sym_params.append(symbolp(arg).?);
    const sym_name = symbolp(name).?;
    const func = try common.newFunctionValue(try common.newFunction(
        sym_name,
        try sym_params.toOwnedSlice(),
        body,
        env,
    ));
    return .{ func, try putPure(env, sym_name, func) };
}

// (define id expr)
fn defineValue() anyerror!ValueRef {
    unreachable;
}

fn lambda(args: []ValueRef, env: Map) anyerror!EvalResult {
    const params = args[0];
    const body = args[1..];
    var sym_params = std.ArrayList(Symbol.ID).init(alloc);
    {
        var tmp = try toSlice(params);
        for (tmp) |a| try sym_params.append(symbolp(a).?);
    }
    const func = try common.newFunctionValue(try common.newFunction(
        null,
        try sym_params.toOwnedSlice(),
        body,
        env,
    ));
    return .{ func, env };
}

// special form
fn begin(args: []ValueRef, env: Map) anyerror!EvalResult {
    var ret = common.empty();
    var new_env = try env.clone();
    for (args) |p| ret, new_env = try evaluate(p, new_env);
    return .{ ret, new_env }; // return the last result
}

// built-in func
fn car(args: []ValueRef) anyerror!ValueRef {
    return args[0].cons.car;
}

// built-in func
fn cdr(args: []ValueRef) anyerror!ValueRef {
    return args[0].cons.cdr;
}

// built-in func
fn cons_(args: []ValueRef) anyerror!ValueRef {
    return common.newConsValue(args[0], args[1]);
}

// built-in func
fn list(xs: []ValueRef) anyerror!ValueRef {
    var ret = common.empty();
    var i = xs.len;
    while (i > 0) {
        i -= 1;
        ret = try common.newConsValue(xs[i], ret);
    }
    return ret;
}

// built-in func
fn add(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs) |x| ret += numberp(x).?;
    return common.newNumberValue(ret);
}

// built-in func
fn sub(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| {
        if (i == 0) ret += numberp(x).? else ret -= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn mul(xs: []ValueRef) anyerror!ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn quotient(xs: []ValueRef) anyerror!ValueRef {
    const a = numberp(xs[0]).?;
    const b = numberp(xs[1]).?;
    return common.newNumberValue(@divFloor(a, b));
}

// built-in func
fn modulo(xs: []ValueRef) anyerror!ValueRef {
    const a = numberp(xs[0]).?;
    const b = numberp(xs[1]).?;
    return common.newNumberValue(@mod(a, b));
}

// built-in func
fn or_(xs: []ValueRef) anyerror!ValueRef {
    for (xs) |x| if (toBool(x)) return t();
    return f();
}

// built-in func
fn and_(xs: []ValueRef) anyerror!ValueRef {
    for (xs) |x| if (!toBool(x)) return f();
    return t();
}

// built-in func
fn eq(args: []ValueRef) anyerror!ValueRef {
    if (deepEql(args[0], args[1])) return t();
    return f();
}

fn toValue(x: bool) anyerror!ValueRef {
    return if (x) t() else common.f();
}

// built-in func
fn le(args: []ValueRef) anyerror!ValueRef {
    return toValue(args[0].number < args[1].number);
}

// built-in func
fn length(x: []ValueRef) anyerror!ValueRef {
    const slice = try toSlice(x[0]);
    return common.newNumberValue(@intCast(slice.len));
}

// built-in func
fn null_(x: []ValueRef) anyerror!ValueRef {
    return toValue(x[0] == common.empty());
}

// built-in func
fn print(xs: []ValueRef) !ValueRef {
    for (xs) |x| {
        const str = try common.toString(x);
        const stdout = std.io.getStdOut().writer();
        nosuspend try stdout.print("#print: {s}\n", .{str});
    }
    return xs[xs.len - 1];
}

fn atomp(cons: ValueRef) ?ValueRef {
    switch (cons.*) {
        Value.cons => return null,
        else => return cons,
    }
}

fn numberp(atom: ValueRef) ?i64 {
    switch (atom.*) {
        Value.number => |num| return num,
        else => return null,
    }
}

fn symbolp(atom: ValueRef) ?Symbol.ID {
    switch (atom.*) {
        Value.symbol => |sym| return sym,
        else => return null,
    }
}

/// The "deep equal" function for values.
/// Equality of function values are only based on the names.
pub fn deepEql(x: ValueRef, y: ValueRef) bool {
    if (x == common.empty() or y == common.empty()) return x == y;
    switch (x.*) {
        Value.number => |x_| switch (y.*) {
            Value.number => |y_| return x_ == y_,
            else => return false,
        },
        Value.symbol => |x_| switch (y.*) {
            Value.symbol => |y_| return x_ == y_,
            else => return false,
        },
        Value.cons => |x_| switch (y.*) {
            Value.cons => |y_| return deepEql(x_.car, y_.car) and deepEql(x_.cdr, y_.cdr),
            else => return false,
        },
        Value.function => |x_| switch (y.*) {
            Value.function => |y_| {
                if (x_.name != null and y_.name != null) return x_.name.? == y_.name.?; // just comparing name
                if (x_.name == null and y_.name == null) return true;
                return false;
            },
            else => return false,
        },
    }
}

fn putPure(env: Map, key: Symbol.ID, val: ValueRef) !Map {
    var ret = try env.clone();
    try ret.put(key, val);
    return ret;
}
