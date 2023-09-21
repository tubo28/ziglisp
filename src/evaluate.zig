const std = @import("std");

const common = @import("common.zig");
const alloc = common.alloc;
const Function = common.Function;
const Map = common.Map;
const nil = common.nil;
const tos = common.toStringDot;
const Value = common.Value;

const Token = @import("tokenize.zig").Token;

pub fn evaluate(x: *const Value, env: *Map) *const Value {
    //     std.log.debug("evaluate arg: {s}", .{tos(x)});
    if (isNil(x)) return x;
    switch (x.*) {
        Value.number, Value.function => return x,
        Value.symbol => |sym| {
            if (env.get(sym)) |ent| return ent;
            return x;
        },
        Value.cons => {
            const car = _car(x);
            switch (car.*) {
                Value.number => @panic("number cannot be a function"),
                Value.cons => @panic("cons cell cannot be a function"),
                Value.function => @panic("unimplemented"),
                Value.symbol => |sym| {
                    const eql = std.mem.eql;

                    // Special forms
                    // TODO: Rewrite some of below with macro. They are macro (not special form) in many LISP dialects.
                    {
                        const args = toSlice(_cdr(x));
                        if (eql(u8, sym, "quote"))
                            return args[0];
                        if (eql(u8, sym, "progn"))
                            return progn(_cdr(x), env);
                        if (eql(u8, sym, "setq"))
                            return setq(_cdr(x), env);
                        if (eql(u8, sym, "defun"))
                            return defun(args[0], args[1], args[2], env);
                        if (eql(u8, sym, "if"))
                            return _if(args[0], args[1], if (args.len >= 3) args[2] else null, env);
                    }

                    // User-defined functions
                    if (env.get(sym)) |func| {
                        const f = func.function;
                        const args = toEvaledSlice(_cdr(x), env);
                        return call(f, args);
                    }

                    // Built-in functions
                    // TODO: Move some of them to another file and read it with @embedFile
                    {
                        const args = toEvaledSlice(_cdr(x), env);
                        if (eql(u8, sym, "car"))
                            return _car(args[0]);
                        if (eql(u8, sym, "cdr"))
                            return _cdr(args[0]);
                        if (eql(u8, sym, "cons"))
                            return _cons(args[0], args[1]);
                        if (eql(u8, sym, "print"))
                            return _print(args[0]);
                        if (eql(u8, sym, "+"))
                            return _add(args);
                        if (eql(u8, sym, "length"))
                            return _length(args[0]);
                    }

                    std.log.err("function or special form not defined: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
}

fn call(func: *const Function, args: []*const Value) *const Value {
    // defunをパースするとき -> その時のenvを渡し、functionオブジェクトで持つ
    // defunを呼ぶとき -> その時のenvを渡さず、functionオブジェクトが持っているenvを使う ただし引数だけ追加（上書き）する
    if (func.params.len != args.len) {
        std.log.err("wrong number of argument for {s}", .{func.name});
        @panic("wrong number of arguments");
    }

    var newEnv = func.env.clone() catch unreachable;
    for (func.params, args) |param, arg|
        newEnv.put(param, arg) catch unreachable; // overwrite if entry exist

    return evaluate(func.body, &newEnv);
}

// Convert list like (foo bar buz) to slice
fn toSlice(head: *const Value) []*const Value {
    var ret = std.ArrayList(*const Value).init(alloc); // defer deinit?
    var h = head;
    while (h != nil()) {
        const x = _car(h);
        ret.append(x) catch @panic("cannot append");
        h = _cdr(h);
    }
    return ret.toOwnedSlice() catch unreachable;
}

fn toEvaledSlice(head: *const Value, env: *Map) []*const Value {
    var ret = toSlice(head);
    for (ret) |*x| x.* = evaluate(x.*, env);
    return ret;
}

// special form
fn _if(cond: *const Value, t: *const Value, f: ?*const Value, env: *Map) *const Value {
    if (!isNil(evaluate(cond, env))) return evaluate(t, env);
    if (f) |ff| return evaluate(ff, env);
    return nil();
}

fn isNil(x: *const Value) bool {
    if (x == nil()) return true;
    switch (x.*) {
        Value.cons => |cons| if (cons.car == nil() and cons.cdr == nil()) return true else return false,
        else => return false,
    }
}

// special form
// scope is lexical i.e. 'env' is the snapshot of parse's env
// TODO: defun can be rewritten using lambda and macro
fn defun(name: *const Value, params: *const Value, body: *const Value, env: *Map) *const Value {
    // defunをパースするとき -> その時のenvを渡し、functionオブジェクトで持つ
    // defunを呼ぶとき -> その時のenvを渡さず、functionオブジェクトが持っているenvを使う
    var paramSymbols = std.ArrayList([]const u8).init(alloc);
    {
        var tmp = toSlice(params);
        for (tmp) |a| paramSymbols.append(_symbolp(a).?) catch unreachable;
    }
    const nameSymbol = _symbolp(name).?;
    const func = common.newFuncValue(
        nameSymbol,
        paramSymbols.toOwnedSlice() catch unreachable,
        body,
        env,
    );
    env.put(nameSymbol, func) catch unreachable;
    return func;
}

// special form
fn setq(x: *const Value, env: *Map) *const Value {
    if (isNil(x)) return nil(); // TODO: use toSlice
    const sym = _symbolp(_car(x)).?;
    const val = evaluate(_car(_cdr(x)), env);
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
fn _car(cons: *const Value) *const Value {
    return cons.cons.car;
}

// built-in func
fn _cdr(cons: *const Value) *const Value {
    return cons.cons.cdr;
}

// built-in func
fn _cons(car: *const Value, cdr: *const Value) *const Value {
    return common.newConsValue(car, cdr);
}

// built-in func
fn _atomp(cons: *const Value) ?*const Value {
    switch (cons.*) {
        Value.cons => return null,
        else => return cons,
    }
}

// built-in func
fn _numberp(atom: *const Value) ?i64 {
    switch (atom.*) {
        Value.number => |num| return num,
        else => return null,
    }
}

// built-in func
fn _symbolp(atom: *const Value) ?[]const u8 {
    switch (atom.*) {
        Value.symbol => |sym| return sym,
        else => return null,
    }
}

// built-in func
fn _add(xs: []*const Value) *const Value {
    var ret: i64 = 0;
    for (xs) |x| ret += _numberp(x).?;
    return common.newAtomValue(i64, ret);
}

// built-in func
fn _length(x: *const Value) *const Value {
    const slice = toSlice(x);
    return common.newAtomValue(i64, @intCast(slice.len));
}

// built-in func
fn _print(x: *const Value) *const Value {
    const str = common.toStringDot(x);
    const stdout = std.io.getStdOut().writer();
    nosuspend stdout.print("#print: {s}\n", .{str}) catch unreachable;
    return x;
}
