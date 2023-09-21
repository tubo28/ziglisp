const std = @import("std");
pub const Map = std.StringHashMap(*const Value);

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const alloc = gpa.allocator();

const Cons = struct {
    car: *const Value,
    cdr: *const Value,
};

pub fn newCons(car: *const Value, cdr: *const Value) *Cons {
    var cons: *Cons = alloc.create(Cons) catch unreachable;
    cons.* = Cons{ .car = car, .cdr = cdr };
    return cons;
}

// Node of tree.
// On tree, it is a branch if cons, otherwise leaf
const ValueTag = enum {
    number,
    symbol,
    cons,
    function,
};

pub const Value = union(ValueTag) {
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

pub fn newAtomValue(comptime T: type, value: T) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    switch (T) {
        i64 => ret.* = Value{ .number = value },
        []const u8 => ret.* = Value{ .symbol = value },
        else => @panic("currently atom of only i64 or string are implemented"),
    }
    return ret;
}

pub fn newFuncValue(name: []const u8, params: [][]const u8, body: *const Value, env: *const Map) *Value {
    var ret: *Value = alloc.create(Value) catch unreachable;
    ret.* = Value{ .function = newFunc(name, params, body, env) };
    return ret;
}

const Function = struct {
    name: []const u8,
    params: [][]const u8,
    body: *const Value,
    env: *const Map, // captured env. scope?
};

pub fn newFunc(name: []const u8, params: [][]const u8, body: *const Value, env: *const Map) *Function {
    var ret: *Function = alloc.create(Function) catch unreachable;
    ret.* = Function{
        .name = name,
        .params = params,
        .body = body,
        .env = env,
    };
    return ret;
}

var _nil: ?*Value = null;

// nil is a ConsCell such that both its car and cdr are itself.
pub fn nil() *const Value {
    if (_nil) |n| {
        return n;
    } else {
        var n: *Value = alloc.create(Value) catch @panic("errro");
        n.* = Value{ .cons = newCons(n, n) };
        _nil = n;
        return nil();
    }
}

pub fn toStringDot(cell: *const Value) []const u8 {
    var builder = std.ArrayList(u8).init(alloc);
    defer builder.deinit();
    toStringDotInner(cell, &builder);
    return builder.toOwnedSlice() catch unreachable;
}

fn toStringDotInner(cell: *const Value, builder: *std.ArrayList(u8)) void {
    if (cell == nil()) {
        builder.appendSlice("nil") catch unreachable;
        return;
    }
    switch (cell.*) {
        Value.cons => |cons| {
            builder.appendSlice("(") catch unreachable;
            toStringDotInner(cons.car, builder);
            builder.appendSlice(" . ") catch unreachable;
            toStringDotInner(cons.cdr, builder);
            builder.appendSlice(")") catch unreachable;
        },
        Value.number => |num| {
            var buffer: [30]u8 = undefined;
            const str = std.fmt.bufPrint(buffer[0..], "{}", .{num}) catch @panic("too large integer");
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
