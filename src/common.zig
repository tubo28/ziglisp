const std = @import("std");

pub const ValueRef = *const Value;

pub const Map = std.StringHashMap(ValueRef);

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const alloc = gpa.allocator();

pub const Cons = struct {
    car: ValueRef,
    cdr: ValueRef,
};

pub fn newCons(car: ValueRef, cdr: ValueRef) *Cons {
    var cons: *Cons = alloc.create(Cons) catch unreachable;
    cons.* = Cons{ .car = car, .cdr = cdr };
    return cons;
}

/// Node of tree.
/// It is a branch only if cons, otherwise leaf.
pub const Value = union(enum) {
    number: i64,
    symbol: []const u8,
    cons: *Cons,
    function: *Function,
};

pub fn newConsValue(car: ValueRef, cdr: ValueRef) ValueRef {
    var ret = alloc.create(Value) catch unreachable;
    ret.* = Value{ .cons = newCons(car, cdr) };
    return ret;
}

pub fn newNumberValue(x: i64) ValueRef {
    return newAtomValue(i64, x);
}

pub fn newSymbolValue(x: []const u8) ValueRef {
    return newAtomValue([]const u8, x);
}

fn newAtomValue(comptime T: type, value: T) ValueRef {
    var ret = alloc.create(Value) catch unreachable;
    switch (T) {
        i64 => ret.* = Value{ .number = value },
        []const u8 => ret.* = Value{ .symbol = value },
        else => @panic("currently atom of only i64 or string are implemented"),
    }
    return ret;
}

pub fn newFunctionValue(
    name: []const u8,
    params: [][]const u8,
    body: []ValueRef,
    env: Map,
) ValueRef {
    var ret = alloc.create(Value) catch unreachable;
    ret.* = Value{ .function = newFunc(name, params, body, env) };
    return ret;
}

pub const Function = struct {
    name: []const u8,
    params: [][]const u8,
    body: []ValueRef, // TODO: Make this single
    env: Map, // captured env (lexical scope)
};

pub fn newFunc(name: []const u8, params: [][]const u8, body: []ValueRef, env: Map) *Function {
    var ret: *Function = alloc.create(Function) catch unreachable;
    ret.* = Function{
        .name = name,
        .params = params,
        .body = body,
        .env = env,
    };
    return ret;
}

var nil_opt: ?*Value = null;
var t_opt: ?*Value = null;

/// nil is a ConsCell such that both its car and cdr are itself.
pub fn nil() ValueRef {
    if (nil_opt) |n| return n;
    var n = alloc.create(Value) catch unreachable;
    n.* = Value{ .cons = newCons(n, n) };
    nil_opt = n;
    return nil_opt.?;
}

pub fn t() ValueRef {
    if (t_opt) |tt| return tt;
    var t_ = alloc.create(Value) catch unreachable;
    t_.* = Value{ .symbol = "t" };
    t_opt = t_;
    return t_opt.?;
}

pub fn toString(cell: ValueRef) []const u8 {
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();
    toStringInner(cell, &buf);
    return buf.toOwnedSlice() catch unreachable;
}

fn toStringInner(cell: ValueRef, builder: *std.ArrayList(u8)) void {
    if (cell == nil()) {
        builder.appendSlice("nil") catch unreachable;
        return;
    }
    switch (cell.*) {
        Value.cons => consToString(cell, builder),
        Value.number => |num| {
            var buffer: [30]u8 = undefined;
            const str = std.fmt.bufPrint(
                buffer[0..],
                "{}",
                .{num},
            ) catch @panic("too large integer");
            builder.appendSlice(str) catch unreachable;
        },
        Value.symbol => |sym| builder.appendSlice(sym) catch unreachable,
        Value.function => |func| {
            builder.appendSlice("<function:") catch unreachable;
            builder.appendSlice(func.name) catch unreachable;
            builder.appendSlice(">") catch unreachable;
        },
    }
}

fn consToString(x: ValueRef, builder: *std.ArrayList(u8)) void {
    switch (consOpt(x).?.cdr.*) {
        Value.cons => {
            // List
            builder.append('(') catch unreachable;
            var head = x;
            var first = true;
            while (head != nil()) {
                if (consOpt(head)) |c| {
                    if (!first) builder.append(' ') catch unreachable;
                    first = false;
                    toStringInner(c.car, builder);
                    head = c.cdr;
                } else {
                    break;
                }
            }
            builder.append(')') catch unreachable;
        },
        else => {
            // Dotted pair
            builder.append('(') catch unreachable;
            toStringInner(x.cons.car, builder);
            builder.appendSlice(" . ") catch unreachable;
            toStringInner(x.cons.cdr, builder);
            builder.append(')') catch unreachable;
        },
    }
}

fn consOpt(x: ValueRef) ?*const Cons {
    return switch (x.*) {
        Value.cons => |cons| cons,
        else => null,
    };
}
