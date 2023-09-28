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

const Token = @import("tokenize.zig").Token;

pub const EvalResult = struct { ValueRef, Map };

pub fn evaluate(x: ValueRef, env: Map) anyerror!EvalResult {
    switch (x.*) {
        Value.number => return .{ x, env },
        Value.function => unreachable,
        Value.symbol => |sym| {
            return if (env.get(sym)) |ent| .{ ent, env } else .{ x, env };
        },
        Value.cons => |cons| {
            if (x == common.empty()) {
                std.log.err("empty list cannot be evaluated", .{});
                unreachable;
            }
            switch (cons.car.*) {
                Value.number => @panic("number cannot be a function"),
                Value.cons => {
                    const l, _ = try evaluate(car(x), env);
                    const ar, _ = try toEvaledSlice(cdr(x), env);
                    return callFunction(l, ar);
                },
                Value.function => @panic("unimplemented"),
                Value.symbol => |sym| {
                    const eql = std.mem.eql;

                    // Special forms.
                    {
                        const args = try toSlice(cdr(x));
                        if (eql(u8, sym, "quote"))
                            return .{ args[0], env };
                        if (eql(u8, sym, "begin"))
                            return begin(cdr(x), env);
                        if (eql(u8, sym, "setq"))
                            return setq(cdr(x), env);
                        if (eql(u8, sym, "defun"))
                            return defun(args[0], args[1], args[2..], env);
                        if (eql(u8, sym, "lambda"))
                            return lambda(args[0], args[1..], env);
                        if (eql(u8, sym, "if"))
                            return if_(args[0], args[1], if (args.len >= 3) args[2] else null, env);
                        if (eql(u8, sym, "cond"))
                            return cond(args, env);
                        if (eql(u8, sym, "let"))
                            return let(args[0], args[1], env);
                    }

                    // User-defined functions.
                    if (env.get(sym)) |func| {
                        const args, const new_env = try toEvaledSlice(cdr(x), env);
                        switch (func.*) {
                            Value.function => return callFunction(func, args),
                            Value.cons => {
                                const ef, _ = try evaluate(func, new_env);
                                return callFunction(ef, args);
                            },
                            else => @panic("symbol not binded to function"),
                        }
                    }

                    // Built-in functions.
                    // TODO: Move some of them to another file and read it with @embedFile
                    builtin: {
                        // Traverse the list of built-in functions to check the existence of function
                        // in order not to call toEvaledSlice meaninglessly.
                        const names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "/", "=", "<", "<=", ">", ">=", "or", "and", "length", "null?" };
                        var found = false;
                        for (names) |n| found = found or eql(u8, sym, n);
                        if (!found) break :builtin;

                        const args, const new_env = try toEvaledSlice(cdr(x), env);
                        if (eql(u8, sym, "car"))
                            return .{ car(args[0]), new_env };
                        if (eql(u8, sym, "cdr"))
                            return .{ cdr(args[0]), new_env };
                        if (eql(u8, sym, "cons"))
                            return .{ try cons_(args[0], args[1]), new_env };
                        if (eql(u8, sym, "list"))
                            return .{ try list(args), new_env };
                        if (eql(u8, sym, "print"))
                            return .{ try print(args[0]), new_env };
                        if (eql(u8, sym, "+"))
                            return .{ try add(args), new_env };
                        if (eql(u8, sym, "-"))
                            return .{ try sub(args), new_env };
                        if (eql(u8, sym, "*"))
                            return .{ try mul(args), new_env };
                        if (eql(u8, sym, "/"))
                            return .{ try div(args), new_env };
                        if (eql(u8, sym, "="))
                            return .{ eq(args[0], args[1]), new_env };
                        if (eql(u8, sym, "<"))
                            return .{ le(args[0], args[1]), new_env };
                        if (eql(u8, sym, "<="))
                            return .{ leq(args[0], args[1]), new_env };
                        if (eql(u8, sym, ">"))
                            return .{ ge(args[0], args[1]), new_env };
                        if (eql(u8, sym, ">="))
                            return .{ geq(args[0], args[1]), new_env };
                        if (eql(u8, sym, "or"))
                            return .{ or_(args), new_env };
                        if (eql(u8, sym, "and"))
                            return .{ and_(args), new_env };
                        if (eql(u8, sym, "length"))
                            return .{ try length(args[0]), new_env };
                        if (eql(u8, sym, "null?"))
                            return .{ null_(args[0]), new_env };
                    }

                    std.log.err("function or special form not defined: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
}

fn callFunction(x: ValueRef, args: []ValueRef) anyerror!EvalResult {
    const func = x.function;
    if (func.params.len != args.len) {
        std.log.err("wrong number of argument for {s}", .{func.name orelse "lambda"});
        @panic("wrong number of arguments for function");
    }

    var new_env = try func.env.clone();
    // Evaluate arguments.
    // Overwrite env entries if exist.
    // It means argument name shadows the values with same name at the defined time.
    // For function name
    if (func.name) |name| try new_env.put(name, x);
    // For arguments
    for (func.params, args) |param, arg|
        try new_env.put(param, arg);

    // Eval body.
    std.debug.assert(func.body.len != 0);
    var ret: ValueRef = undefined;
    for (func.body) |expr| ret, new_env = try evaluate(expr, new_env);
    return .{ ret, new_env };
}

// Convert sequence of cons cell like (foo bar buz) to slice.
fn toSlice(head: ValueRef) ![]ValueRef {
    var ret = std.ArrayList(ValueRef).init(alloc); // defer deinit?
    var h = head;
    while (h != common.empty()) {
        const x = car(h);
        ret.append(x) catch @panic("cannot append");
        h = cdr(h);
    }
    return try ret.toOwnedSlice();
}

fn toEvaledSlice(head: ValueRef, env: Map) !struct { []ValueRef, Map } {
    var ret = try toSlice(head);
    var e = try env.clone();
    for (ret) |*x| x.*, e = try evaluate(x.*, env);
    return .{ ret, e };
}

// special form
fn if_(pred: ValueRef, then: ValueRef, unless: ?ValueRef, env: Map) anyerror!EvalResult {
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
fn let(pairs: ValueRef, expr: ValueRef, env: Map) anyerror!EvalResult {
    const pairsSlice = try toSlice(pairs);
    const n = pairsSlice.len;

    var keys: [][]const u8 = try alloc.alloc([]const u8, n);
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
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
fn defun(name: ValueRef, params: ValueRef, body: []ValueRef, env: Map) anyerror!EvalResult {
    std.debug.assert(body.len != 0); // Ill-formed special form

    var sym_params = std.ArrayList([]const u8).init(alloc);
    {
        var tmp = try toSlice(params);
        for (tmp) |a| try sym_params.append(symbolp(a).?);
    }
    const sym_name = symbolp(name).?;
    const func = try common.newFunctionValue(
        sym_name,
        try sym_params.toOwnedSlice(),
        body,
        env,
    );
    return .{ func, try putPure(env, sym_name, func) };
}

fn lambda(params: ValueRef, body: []ValueRef, env: Map) anyerror!EvalResult {
    var sym_params = std.ArrayList([]const u8).init(alloc);
    {
        var tmp = try toSlice(params);
        for (tmp) |a| try sym_params.append(symbolp(a).?);
    }
    const func = try common.newFunctionValue(
        null,
        try sym_params.toOwnedSlice(),
        body,
        env,
    );
    return .{ func, env };
}

// special form
fn setq(x: ValueRef, env: Map) anyerror!EvalResult {
    const slice = try toSlice(x);
    if (slice.len != 2) @panic("argument for setq was not 2");
    const sym = symbolp(slice[0]).?;
    const val, var new_env = try evaluate(slice[1], env);
    try new_env.put(sym, val);
    return .{ val, new_env };
}

// special form
fn begin(x: ValueRef, env: Map) anyerror!EvalResult {
    const slice = try toSlice(x);
    var ret = common.empty();
    var new_env = try env.clone();
    for (slice) |p| ret, new_env = try evaluate(p, new_env);
    return .{ ret, new_env }; // return the last result
}

// built-in func
fn car(x: ValueRef) ValueRef {
    return x.cons.car;
}

// built-in func
fn cdr(x: ValueRef) ValueRef {
    return x.cons.cdr;
}

// built-in func
fn cons_(car_: ValueRef, cdr_: ValueRef) !ValueRef {
    return common.newConsValue(car_, cdr_);
}

// built-in func
fn list(xs: []ValueRef) !ValueRef {
    var ret = common.empty();
    var i = xs.len;
    while (i > 0) {
        i -= 1;
        ret = try common.newConsValue(xs[i], ret);
    }
    return ret;
}

// built-in func
fn atomp(cons: ValueRef) ?ValueRef {
    switch (cons.*) {
        Value.cons => return null,
        else => return cons,
    }
}

// built-in func
fn numberp(atom: ValueRef) ?i64 {
    switch (atom.*) {
        Value.number => |num| return num,
        else => {
            std.log.debug("not a number, {}", .{atom});
            return null;
        },
    }
}

// built-in func
fn symbolp(atom: ValueRef) ?[]const u8 {
    switch (atom.*) {
        Value.symbol => |sym| return sym,
        else => return null,
    }
}

// built-in func
fn add(xs: []ValueRef) !ValueRef {
    var ret: i64 = 0;
    for (xs) |x| ret += numberp(x).?;
    return common.newNumberValue(ret);
}

// built-in func
fn sub(xs: []ValueRef) !ValueRef {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| {
        if (i == 0) ret += numberp(x).? else ret -= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn mul(xs: []ValueRef) !ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn div(xs: []ValueRef) !ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).? else ret = @divFloor(ret, numberp(x).?);
    }
    return common.newNumberValue(ret);
}

// built-in func
fn or_(xs: []ValueRef) ValueRef {
    for (xs) |x| if (toBool(x)) return t();
    return f();
}

// built-in func
fn and_(xs: []ValueRef) ValueRef {
    for (xs) |x| if (!toBool(x)) return f();
    return t();
}

// built-in func
fn eq(x: ValueRef, y: ValueRef) ValueRef {
    if (deepEql(x, y)) return t();
    return f();
}

fn toValue(x: bool) ValueRef {
    return if (x) t() else common.f();
}

// built-in func
fn le(x: ValueRef, y: ValueRef) ValueRef {
    return toValue(x.number < y.number);
}

// built-in func
fn leq(x: ValueRef, y: ValueRef) ValueRef {
    return toValue(x.number <= y.number);
}

// built-in func
fn ge(x: ValueRef, y: ValueRef) ValueRef {
    return toValue(x.number > y.number);
}

// built-in func
fn geq(x: ValueRef, y: ValueRef) ValueRef {
    return toValue(x.number > y.number);
}

// built-in func
fn length(x: ValueRef) !ValueRef {
    const slice = try toSlice(x);
    return common.newNumberValue(@intCast(slice.len));
}

// built-in func
fn null_(x: ValueRef) ValueRef {
    return toValue(x == common.empty());
}

// built-in func
fn print(x: ValueRef) !ValueRef {
    const str = try common.toString(x);
    const stdout = std.io.getStdOut().writer();
    nosuspend try stdout.print("#print: {s}\n", .{str});
    return x;
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
            Value.symbol => |y_| return std.mem.eql(u8, x_, y_),
            else => return false,
        },
        Value.cons => |x_| switch (y.*) {
            Value.cons => |y_| return deepEql(x_.car, y_.car) and deepEql(x_.cdr, y_.cdr),
            else => return false,
        },
        Value.function => |x_| switch (y.*) {
            Value.function => |y_| {
                if (x_.name != null and y_.name != null) return std.mem.eql(u8, x_.name.?, y_.name.?); // just comparing name
                if (x_.name == null and y_.name == null) return true;
                return false;
            },
            else => return false,
        },
    }
}

fn putPure(env: Map, key: []const u8, val: ValueRef) !Map {
    var ret = try env.clone();
    try ret.put(key, val);
    return ret;
}
