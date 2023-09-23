const std = @import("std");

const common = @import("common.zig");
const alloc = common.alloc;
const Function = common.Function;
const Map = common.Map;
const nil = common.nil;
const t = common.t;
const toString = common.toString;
const Value = common.Value;

const Token = @import("tokenize.zig").Token;

// TODO: should env be const? Should evaluate return modified env?
pub fn evaluate(x: *const Value, env: *Map) *const Value {
    if (isNil(x)) return x;
    switch (x.*) {
        Value.number, Value.function => return x,
        Value.symbol => |sym| {
            return if (env.get(sym)) |ent| ent else x;
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
                            return args[0];
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
                        const f = func.function;
                        const args = toEvaledSlice(cdr(x), env);
                        return callFunction(f, args);
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

                        const args = toEvaledSlice(cdr(x), env);
                        if (eql(u8, sym, "car"))
                            return car(args[0]);
                        if (eql(u8, sym, "cdr"))
                            return cdr(args[0]);
                        if (eql(u8, sym, "cons"))
                            return cons_(args[0], args[1]);
                        if (eql(u8, sym, "list"))
                            return list(args);
                        if (eql(u8, sym, "print"))
                            return print(args[0]);
                        if (eql(u8, sym, "+"))
                            return add(args);
                        if (eql(u8, sym, "-"))
                            return sub(args);
                        if (eql(u8, sym, "*"))
                            return mul(args);
                        if (eql(u8, sym, "/"))
                            return div(args);
                        if (eql(u8, sym, "="))
                            return eq(args[0], args[1]);
                        if (eql(u8, sym, "<"))
                            return le(args[0], args[1]);
                        if (eql(u8, sym, "<="))
                            return leq(args[0], args[1]);
                        if (eql(u8, sym, ">"))
                            return ge(args[0], args[1]);
                        if (eql(u8, sym, ">="))
                            return geq(args[0], args[1]);
                        if (eql(u8, sym, "or"))
                            return or_(args);
                        if (eql(u8, sym, "and"))
                            return and_(args);
                        if (eql(u8, sym, "length"))
                            return length(args[0]);
                        if (eql(u8, sym, "null"))
                            return null_(args[0]);
                    }

                    std.log.err("function or special form not defined: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
}

fn callFunction(func: *const Function, args: []*const Value) *const Value {
    if (func.params.len != args.len) {
        std.log.err("wrong number of argument for {s}", .{func.name});
        @panic("wrong number of arguments");
    }

    var new_env = func.env.clone() catch unreachable;
    for (func.params, args) |param, arg|
        new_env.put(param, arg) catch unreachable; // overwrite if entry exist

    var ret = nil();
    for (func.body) |expr| ret = evaluate(expr, &new_env);
    return ret;
}

// Convert sequence of cons cell like (foo bar buz) to slice.
fn toSlice(head: *const Value) []*const Value {
    var ret = std.ArrayList(*const Value).init(alloc); // defer deinit?
    var h = head;
    while (h != nil()) {
        const x = car(h);
        ret.append(x) catch @panic("cannot append");
        h = cdr(h);
    }
    return ret.toOwnedSlice() catch unreachable;
}

fn toEvaledSlice(head: *const Value, env: *Map) []*const Value {
    var ret = toSlice(head);
    for (ret) |*x| x.* = evaluate(x.*, env);

    return ret;
}

// special form
fn if_(cond_: *const Value, then: *const Value, unless: ?*const Value, env: *Map) *const Value {
    if (toBool(evaluate(cond_, env))) return evaluate(then, env);
    if (unless) |f| return evaluate(f, env);
    return nil();
}

// special form
fn cond(clauses: []*const Value, env: *Map) *const Value {
    for (clauses) |c| {
        const tmp = toSlice(c);
        const cond_ = tmp[0];
        const then = tmp[1];
        if (toBool(evaluate(cond_, env)))
            return evaluate(then, env);
    }
    return nil();
}

fn toBool(x: *const Value) bool {
    return !isNil(x);
}

// TODO: rename this to null_
fn isNil(x: *const Value) bool {
    if (x == nil()) return true;
    switch (x.*) {
        Value.cons => |cons| if (cons.car == nil() and cons.cdr == nil()) return true else return false,
        else => return false,
    }
}

// special form
// TODO: Make env const.
fn let(vals: *const Value, expr: *const Value, env: *Map) *const Value {
    const values = toSlice(vals);
    var new_env = env.clone() catch unreachable;
    for (values) |v| {
        const keyVal = toSlice(v);
        const key = keyVal[0].symbol;
        const val = evaluate(keyVal[1], env);
        new_env.put(key, val) catch unreachable;
    }
    return evaluate(expr, &new_env);
}

// special form
// The scope is lexical, i.e., the 'env' of returned value is a snapshot of the parser's env.
// TODO: Defun can be rewritten using lambda and macro.
fn defun(name: *const Value, params: *const Value, body: []*const Value, env: *Map) *const Value {
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
    env.put(sym_name, func) catch unreachable;
    return func;
}

// special form
fn setq(x: *const Value, env: *Map) *const Value {
    if (isNil(x)) return nil(); // TODO: use toSlice
    const sym = symbolp(car(x)).?;
    const val = evaluate(car(cdr(x)), env);
    env.put(sym, val) catch unreachable;
    return val;
}

// special form
fn progn(x: *const Value, env: *Map) *const Value {
    const slice = toSlice(x);
    var ret = nil();
    for (slice) |p| ret = evaluate(p, env);
    return ret; // the last result of 'evaluate' is returned
}

// built-in func
fn car(x: *const Value) *const Value {
    return x.cons.car;
}

// built-in func
fn cdr(x: *const Value) *const Value {
    return x.cons.cdr;
}

// built-in func
fn cons_(car_: *const Value, cdr_: *const Value) *const Value {
    return common.newConsValue(car_, cdr_);
}

// built-in func
fn list(xs: []*const Value) *const Value {
    var ret = nil();
    var i = xs.len;
    while (i > 0) {
        i -= 1;
        ret = common.newConsValue(xs[i], ret);
    }
    return ret;
}

// built-in func
fn atomp(cons: *const Value) ?*const Value {
    switch (cons.*) {
        Value.cons => return null,
        else => return cons,
    }
}

// built-in func
fn numberp(atom: *const Value) ?i64 {
    switch (atom.*) {
        Value.number => |num| return num,
        else => return null,
    }
}

// built-in func
fn symbolp(atom: *const Value) ?[]const u8 {
    switch (atom.*) {
        Value.symbol => |sym| return sym,
        else => return null,
    }
}

// built-in func
fn add(xs: []*const Value) *const Value {
    var ret: i64 = 0;
    for (xs) |x| ret += numberp(x).?;
    return common.newNumberValue(ret);
}

// built-in func
fn sub(xs: []*const Value) *const Value {
    var ret: i64 = 0;
    for (xs, 0..) |x, i| {
        if (i == 0) ret += numberp(x).? else ret -= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn mul(xs: []*const Value) *const Value {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).?;
    }
    return common.newNumberValue(ret);
}

// built-in func
fn div(xs: []*const Value) *const Value {
    var ret: i64 = 1;
    for (xs, 0..) |x, i| {
        if (i == 0) ret *= numberp(x).? else ret = @divFloor(ret, numberp(x).?);
    }
    return common.newNumberValue(ret);
}

// built-in func
fn or_(xs: []*const Value) *const Value {
    for (xs) |x| if (toBool(x)) return t();
    return nil();
}

// built-in func
fn and_(xs: []*const Value) *const Value {
    for (xs) |x| if (!toBool(x)) return nil();
    return t();
}

// built-in func
fn eq(x: *const Value, y: *const Value) *const Value {
    if (deepEql(x, y)) return t();
    return nil();
}

fn boolAsSymbol(x: bool) *const Value {
    return if (x) t() else nil();
}

// built-in func
fn le(x: *const Value, y: *const Value) *const Value {
    return boolAsSymbol(x.number < y.number);
}

// built-in func
fn leq(x: *const Value, y: *const Value) *const Value {
    return boolAsSymbol(x.number <= y.number);
}

// built-in func
fn ge(x: *const Value, y: *const Value) *const Value {
    return boolAsSymbol(x.number > y.number);
}

// built-in func
fn geq(x: *const Value, y: *const Value) *const Value {
    return boolAsSymbol(x.number > y.number);
}

// built-in func
fn length(x: *const Value) *const Value {
    const slice = toSlice(x);
    return common.newNumberValue(@intCast(slice.len));
}

// built-in func
fn null_(x: *const Value) *const Value {
    return boolAsSymbol(isNil(x));
}

// built-in func
fn print(x: *const Value) *const Value {
    const str = common.toString(x);
    const stdout = std.io.getStdOut().writer();
    nosuspend stdout.print("#print: {s}\n", .{str}) catch unreachable;
    return x;
}

/// The "deep equal" function for values.
/// Equality of function values are only based on the names.
pub fn deepEql(x: *const Value, y: *const Value) bool {
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
