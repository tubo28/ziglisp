const std = @import("std");

const common = @import("common.zig");
const alloc = common.alloc;
const Map = common.Map;
const nil = common.nil;
const tos = common.toStringDot;
const Value = common.Value;

const Token = @import("tokenize.zig").Token;

pub fn evaluate(x: *const Value, env: *Map) *const Value {
    //     std.log.debug("evaluate arg: {s}", .{tos(x)});
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
                    // TODO: Replace some of below with macro since they are macro instead of special form in many LISP dialects.
                    {
                        const args = toSlice(_cdr(x));
                        if (eql(u8, sym, "quote"))
                            return args[0];
                        if (eql(u8, sym, "progn"))
                            return progn(_cdr(x), env);
                        if (eql(u8, sym, "setq"))
                            return setq(_cdr(x), env);
                        if (eql(u8, sym, "defun")) {
                            // (defun double (x) (+ x x))
                            return defun(args[0], args[1], args[2], env);
                        }
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

                    std.log.err("function for special form not defined: {s}\n", .{sym});
                    unreachable;
                },
            }
        },
    }
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
// scope is lexical i.e. 'env' is the snapshot of parse's env
fn defun(name: *const Value, params: *const Value, body: *const Value, env: *const Map) *const Value {
    // defunをパースするとき -> その時のenvを渡し、functionオブジェクトで持つ
    // defunを呼ぶとき -> その時のenvを渡さず、functionオブジェクトが持っているenvを使う
    var symbols = std.ArrayList([]const u8).init(alloc);
    var paramsSlice = toSlice(params);
    for (paramsSlice) |a| symbols.append(_symbolp(a).?) catch unreachable;
    return common.newFuncValue(
        _symbolp(name).?,
        symbols.toOwnedSlice() catch unreachable,
        body,
        env,
    );
}

// special form
fn setq(x: *const Value, env: *Map) *const Value {
    if (x == nil()) return nil();
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
