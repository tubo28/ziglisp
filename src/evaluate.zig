const std = @import("std");

const common = @import("common.zig");
const alloc = common.alloc;
const Function = common.Function;
const Map = common.Map;
const nil = common.nil;
const t = common.t;
const toString = common.toString;
const ValueRef = common.ValueRef;
const Value = common.Value;

const Token = @import("tokenize.zig").Token;

pub fn evaluate(x: ValueRef, env: Map) struct { ValueRef, Map } {
    if (isNil(x)) return .{ x, env };
    switch (x.*) {
        Value.number, Value.function => return .{ x, env },
        Value.symbol => |sym| {
            return if (env.get(sym)) |ent| .{ ent, env } else .{ x, env };
        },
        Value.cons => |cons| {
            switch (cons.car.*) {
                Value.number => @panic("number cannot be a function"),
                Value.cons => @panic("cons cell cannot be a function"),
                Value.function => @panic("unimplemented"),
                Value.symbol => |sym| {
                    const eql = std.mem.eql;

                    // Special forms.
                    // TODO: Rewrite some of below by macro.
                    {
                        const args = toSlice(cdr(x));
                        if (eql(u8, sym, "quote"))
                            return .{ args[0], env };
                        if (eql(u8, sym, "progn"))
                            return progn(cdr(x), env);
                        if (eql(u8, sym, "setq"))
                            return setq(cdr(x), env);
                        if (eql(u8, sym, "defun"))
                            return defun(args[0], args[1], args[2..], env);
                        if (eql(u8, sym, "if"))
                            return if_(args[0], args[1], if (args.len >= 3) args[2] else null, env);
                        if (eql(u8, sym, "cond"))
                            return cond(args, env);
                        if (eql(u8, sym, "let"))
                            return let(args[0], args[1], env);
                    }

                    // User-defined functions.
                    if (env.get(sym)) |func| {
                        const args, _ = toEvaledSlice(cdr(x), env);
                        return callFunction(func, args);
                    }

                    // Built-in functions.
                    // TODO: Move some of them to another file and read it with @embedFile
                    builtin: {
                        // Traverse the list of built-in functions to check the existence of function
                        // in order not to call toEvaledSlice meaninglessly.
                        const names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "/", "=", "<", "<=", ">", ">=", "or", "and", "length", "null" };
                        var found = false;
                        for (names) |n| found = found or eql(u8, sym, n);
                        if (!found) break :builtin;

                        const args, const new_env = toEvaledSlice(cdr(x), env);
                        if (eql(u8, sym, "car"))
                            return .{ car(args[0]), new_env };
                        if (eql(u8, sym, "cdr"))
                            return .{ cdr(args[0]), new_env };
                        if (eql(u8, sym, "cons"))
                            return .{ cons_(args[0], args[1]), new_env };
                        if (eql(u8, sym, "list"))
                            return .{ list(args), new_env };
                        if (eql(u8, sym, "print"))
                            return .{ print(args[0]), new_env };
                        if (eql(u8, sym, "+"))
                            return .{ add(args), new_env };
                        if (eql(u8, sym, "-"))
                            return .{ sub(args), new_env };
                        if (eql(u8, sym, "*"))
                            return .{ mul(args), new_env };
                        if (eql(u8, sym, "/"))
                            return .{ div(args), new_env };
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
                            return .{ length(args[0]), new_env };
                        if (eql(u8, sym, "null"))
                            return .{ null_(args[0]), new_env };
                    }

                    std.log.err("function or special form not defined: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
}

fn callFunction(func: ValueRef, args: []ValueRef) struct { ValueRef, Map } {
    const f = func.function;
    if (f.params.len != args.len) {
        std.log.err("wrong number of argument for {s}", .{f.name});
        @panic("wrong number of arguments");
    }

    // Eval arguments.
    var new_env = f.env.clone() catch unreachable;

    // Overwrite env entries if exist.
    // It means argument name shadows the values with same name in caller's env.
    // for function name
    new_env.put(f.name, func) catch unreachable;
    // for arguments
    for (f.params, args) |param, arg|
        new_env.put(param, arg) catch unreachable;

    // Eval body.
    var ret = nil();
    for (f.body) |expr| ret, new_env = evaluate(expr, new_env);
    return .{ ret, new_env };
}

// Convert sequence of cons cell like (foo bar buz) to slice.
fn toSlice(head: ValueRef) []ValueRef {
    var ret = std.ArrayList(ValueRef).init(alloc); // defer deinit?
    var h = head;
    while (h != nil()) {
        const x = car(h);
        ret.append(x) catch @panic("cannot append");
        h = cdr(h);
    }
    return ret.toOwnedSlice() catch unreachable;
}

fn toEvaledSlice(head: ValueRef, env: Map) struct { []ValueRef, Map } {
    var ret = toSlice(head);
    var e = env.clone() catch unreachable;
    for (ret) |*x| x.*, e = evaluate(x.*, env);
    return .{ ret, e };
}

// special form
fn if_(pred: ValueRef, then: ValueRef, unless: ?ValueRef, env: Map) struct { ValueRef, Map } {
    const p, const new_env = evaluate(pred, env);
    if (toBool(p)) return evaluate(then, new_env);
    if (unless) |f| return evaluate(f, new_env);
    return .{ nil(), new_env };
}

// special form
fn cond(clauses: []ValueRef, env: Map) struct { ValueRef, Map } {
    var e = env.clone() catch unreachable;
    for (clauses) |c| {
        const tmp = toSlice(c);
        const pred = tmp[0];
        const then = tmp[1];
        const p, e = evaluate(pred, e);
        if (toBool(p)) return evaluate(then, e);
    }
    return .{ nil(), e };
}

fn toBool(x: ValueRef) bool {
    return !isNil(x);
}

// TODO: rename this to null_
fn isNil(x: ValueRef) bool {
    if (x == nil()) return true;
    switch (x.*) {
        Value.cons => |cons| if (cons.car == nil() and cons.cdr == nil()) return true else return false,
        else => return false,
    }
}

// special form
fn let(pairs: ValueRef, expr: ValueRef, env: Map) struct { ValueRef, Map } {
    const pairsSlice = toSlice(pairs);
    const n = pairsSlice.len;

    var keys: [][]const u8 = alloc.alloc([]const u8, n) catch unreachable;
    defer alloc.free(keys);
    var vals: []ValueRef = alloc.alloc(ValueRef, n) catch unreachable;
    defer alloc.free(vals);

    for (pairsSlice, 0..) |p, i| {
        const keyVal = toSlice(p);
        keys[i] = keyVal[0].symbol;
        vals[i] = keyVal[1];
    }

    // The prior binding is not used to evaluate the following binding,
    // but the result of evaluating the RHS of a prior ones are propagated.
    var new_env = env.clone() catch unreachable;
    for (0..vals.len) |i| vals[i], new_env = evaluate(vals[i], new_env);
    for (keys, vals) |k, v| new_env = putPure(new_env, k, v);
    return evaluate(expr, new_env);
}

// special form
// The scope is lexical, i.e., the returning 'env' value is a snapshot of the parser's env.
// TODO: Defun can be rewritten using lambda and macro.
fn defun(name: ValueRef, params: ValueRef, body: []ValueRef, env: Map) struct { ValueRef, Map } {
    var sym_params = std.ArrayList([]const u8).init(alloc);
    {
        var tmp = toSlice(params);
        for (tmp) |a| sym_params.append(symbolp(a).?) catch unreachable;
    }
    const sym_name = symbolp(name).?;
    const func = common.newFunctionValue(
        sym_name,
        sym_params.toOwnedSlice() catch unreachable,
        body,
        env,
    );
    return .{ func, putPure(env, sym_name, func) };
}

// special form
fn setq(x: ValueRef, env: Map) struct { ValueRef, Map } {
    if (isNil(x)) return .{ nil(), env }; // TODO: use toSlice
    const sym = symbolp(car(x)).?;
    const val, var new_env = evaluate(car(cdr(x)), env);
    new_env.put(sym, val) catch unreachable;
    return .{ val, new_env };
}

// special form
fn progn(x: ValueRef, env: Map) struct { ValueRef, Map } {
    const slice = toSlice(x);
    var ret = nil();
    var new_env = env.clone() catch unreachable;
    for (slice) |p| ret, new_env = evaluate(p, new_env);
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
fn cons_(car_: ValueRef, cdr_: ValueRef) ValueRef {
    return common.newConsValue(car_, cdr_);
}

// built-in func
fn list(xs: []ValueRef) ValueRef {
    var ret = nil();
    var i = xs.len;
    while (i > 0) {
        i -= 1;
        ret = common.newConsValue(xs[i], ret);
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
        else => return null,
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
fn add(xs: []ValueRef) ValueRef {
    var ret: i64 = 0;
    for (xs) |x| ret += numberp(x).?;
    return common.newNumberValue(ret);
}

// built-in func
fn sub(xs: []ValueRef) ValueRef {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| {
        if (i == 0) ret += numberp(x).? else ret -= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn mul(xs: []ValueRef) ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn div(xs: []ValueRef) ValueRef {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).? else ret = @divFloor(ret, numberp(x).?);
    }
    return common.newNumberValue(ret);
}

// built-in func
fn or_(xs: []ValueRef) ValueRef {
    for (xs) |x| if (toBool(x)) return t();
    return nil();
}

// built-in func
fn and_(xs: []ValueRef) ValueRef {
    for (xs) |x| if (!toBool(x)) return nil();
    return t();
}

// built-in func
fn eq(x: ValueRef, y: ValueRef) ValueRef {
    if (deepEql(x, y)) return t();
    return nil();
}

fn boolAsSymbol(x: bool) ValueRef {
    return if (x) t() else nil();
}

// built-in func
fn le(x: ValueRef, y: ValueRef) ValueRef {
    return boolAsSymbol(x.number < y.number);
}

// built-in func
fn leq(x: ValueRef, y: ValueRef) ValueRef {
    return boolAsSymbol(x.number <= y.number);
}

// built-in func
fn ge(x: ValueRef, y: ValueRef) ValueRef {
    return boolAsSymbol(x.number > y.number);
}

// built-in func
fn geq(x: ValueRef, y: ValueRef) ValueRef {
    return boolAsSymbol(x.number > y.number);
}

// built-in func
fn length(x: ValueRef) ValueRef {
    const slice = toSlice(x);
    return common.newNumberValue(@intCast(slice.len));
}

// built-in func
fn null_(x: ValueRef) ValueRef {
    return boolAsSymbol(isNil(x));
}

// built-in func
fn print(x: ValueRef) ValueRef {
    const str = common.toString(x);
    const stdout = std.io.getStdOut().writer();
    nosuspend stdout.print("#print: {s}\n", .{str}) catch unreachable;
    return x;
}

/// The "deep equal" function for values.
/// Equality of function values are only based on the names.
pub fn deepEql(x: ValueRef, y: ValueRef) bool {
    if (x == nil() or y == nil()) return x == y;
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
            Value.function => |y_| return std.mem.eql(u8, x_.name, y_.name), // just comparing name
            else => return false,
        },
    }
}

fn putPure(env: Map, key: []const u8, val: ValueRef) Map {
    var ret = env.clone() catch unreachable;
    ret.put(key, val) catch unreachable;
    return ret;
}
