const std = @import("std");

pub const Token = @import("tokenize.zig").Token;
pub const ValueRef = *const Value;

pub const Map = std.StringHashMap(ValueRef);

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const alloc = gpa.allocator();

pub const Cons = struct {
    car: ValueRef,
    cdr: ValueRef,
};

pub fn newCons(car: ValueRef, cdr: ValueRef) !*Cons {
    var cons: *Cons = try alloc.create(Cons);
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

pub fn newConsValue(car: ValueRef, cdr: ValueRef) !ValueRef {
    var ret = try alloc.create(Value);
    ret.* = Value{ .cons = try newCons(car, cdr) };
    return ret;
}

pub fn newNumberValue(x: i64) !ValueRef {
    return newAtomValue(i64, x);
}

pub fn newSymbolValue(x: []const u8) !ValueRef {
    return try newAtomValue([]const u8, x);
}

fn newAtomValue(comptime T: type, value: T) !ValueRef {
    var ret = try alloc.create(Value);
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
) !ValueRef {
    var ret = try alloc.create(Value);
    ret.* = Value{ .function = try newFunc(name, params, body, env) };
    return ret;
}

pub const Function = struct {
    name: []const u8,
    params: [][]const u8,
    body: []ValueRef, // TODO: Make this single
    env: Map, // captured env (lexical scope)
};

pub fn newFunc(name: []const u8, params: [][]const u8, body: []ValueRef, env: Map) !*Function {
    var ret: *Function = try alloc.create(Function);
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
    var n = alloc.create(Value) catch @panic("failed to alloc nil");
    n.* = Value{ .cons = newCons(n, n) catch @panic("failed to alloc cons of nil") };
    nil_opt = n;
    return nil_opt.?;
}

pub fn t() ValueRef {
    if (t_opt) |tt| return tt;
    var t_ = alloc.create(Value) catch @panic("failed to alloc t");
    t_.* = Value{ .symbol = "t" };
    t_opt = t_;
    return t_opt.?;
}

pub fn toString(cell: ValueRef) ![]const u8 {
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();
    try toStringInner(cell, &buf);
    return try buf.toOwnedSlice();
}

fn toStringInner(cell: ValueRef, builder: *std.ArrayList(u8)) anyerror!void {
    if (cell == nil()) {
        try builder.appendSlice("nil");
        return;
    }
    switch (cell.*) {
        Value.cons => try consToString(cell, builder),
        Value.number => |num| {
            var buffer: [30]u8 = undefined;
            const str = std.fmt.bufPrint(
                buffer[0..],
                "{}",
                .{num},
            ) catch @panic("too large integer");
            try builder.appendSlice(str);
        },
        Value.symbol => |sym| try builder.appendSlice(sym),
        Value.function => |func| {
            try builder.appendSlice("<function:");
            try builder.appendSlice(func.name);
            try builder.appendSlice(">");
        },
    }
}

fn consToString(x: ValueRef, builder: *std.ArrayList(u8)) !void {
    switch (consOpt(x).?.cdr.*) {
        Value.cons => {
            // List
            try builder.append('(');
            var head = x;
            var first = true;
            while (head != nil()) {
                if (consOpt(head)) |c| {
                    if (!first) try builder.append(' ');
                    first = false;
                    try toStringInner(c.car, builder);
                    head = c.cdr;
                } else {
                    break;
                }
            }
            try builder.append(')');
        },
        else => {
            // Dotted pair
            try builder.append('(');
            try toStringInner(x.cons.car, builder);
            try builder.appendSlice(" . ");
            try toStringInner(x.cons.cdr, builder);
            try builder.append(')');
        },
    }
}

fn consOpt(x: ValueRef) ?*const Cons {
    return switch (x.*) {
        Value.cons => |cons| cons,
        else => null,
    };
}

pub fn panicAt(tok: Token, message: []const u8) void {
    const stderr = std.io.getStdErr().writer();
    nosuspend {
        stderr.print("error: {s}\n", .{message});
        stderr.print("error: {s}\n", .{message});
        for (0..tok.index + 7) |_| stderr.print(" ", .{});
        stderr.print("^\n");
    }
    unreachable;
}
