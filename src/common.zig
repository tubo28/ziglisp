const std = @import("std");
pub const Map = std.StringHashMap(*const Value);

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const alloc = gpa.allocator();

pub const Cons = struct {
    car: *const Value,
    cdr: *const Value,
};

pub fn newCons(car: *const Value, cdr: *const Value) *Cons {
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

pub fn newConsValue(car: *const Value, cdr: *const Value) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .cons = newCons(car, cdr) };
    return ret;
}

pub fn newNumberValue(x: i64) *Value {
    return newAtomValue(i64, x);
}

pub fn newSymbolValue(x: []const u8) *Value {
    return newAtomValue([]const u8, x);
}

fn newAtomValue(comptime T: type, value: T) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
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
    body: []*const Value,
    env: *const Map,
) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .function = newFunc(name, params, body, env) };
    return ret;
}

pub const Function = struct {
    name: []const u8,
    params: [][]const u8,
    body: []*const Value,
    env: *const Map, // captured env (lexical scope)
};

pub fn newFunc(name: []const u8, params: [][]const u8, body: []*const Value, env: *const Map) *Function {
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
pub fn nil() *const Value {
    if (nil_opt) |n| return n;

    var n: *Value = newConsValue(undefined, undefined);
    n.cons.car = n;
    n.cons.cdr = n;
    nil_opt = n;
    return nil_opt.?;
}

pub fn t() *const Value {
    if (t_opt) |tt| return tt;
    t_opt = newSymbolValue("t");
    return t_opt.?;
}

pub fn toString(cell: *const Value) []const u8 {
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();
    toStringInner(cell, &buf);
    return buf.toOwnedSlice() catch unreachable;
}

fn toStringInner(cell: *const Value, builder: *std.ArrayList(u8)) void {
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

fn consToString(x: *const Value, builder: *std.ArrayList(u8)) void {
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

fn consOpt(x: *const Value) ?*const Cons {
    return switch (x.*) {
        Value.cons => |cons| cons,
        else => null,
    };
}
