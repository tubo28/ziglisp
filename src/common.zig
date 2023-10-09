const std = @import("std");

const S = @import("symbol.zig");

const SymbolID = S.ID;
const Env = @import("env.zig").Env;
const EnvRef = Env.Ref;

pub const ValueRef = *const Value;
pub const EvalResult = struct { ValueRef, EnvRef };

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const alloc = gpa.allocator();

pub const Cons = struct {
    car: ValueRef,
    cdr: ValueRef,
};

pub fn new(ty: anytype, x: ty) !*ty {
    var ret: *ty = try alloc.create(ty);
    ret.* = x;
    return ret;
}

pub const ValueTag = enum { number, symbol, cons, function, b_func, b_spf };
/// Node of tree.
/// It is a branch only if cons, otherwise leaf.
pub const Value = union(ValueTag) {
    number: i64,
    symbol: SymbolID,
    cons: *const Cons,
    function: *const Function,
    b_func: usize, // Index of table
    b_spf: usize,
};

pub fn newCons(car: ValueRef, cdr: ValueRef) !ValueRef {
    return new(
        Value,
        Value{ .cons = try new(Cons, Cons{ .car = car, .cdr = cdr }) },
    );
}

pub const Function = struct {
    name: ?SymbolID, // null for lambda
    params: []SymbolID,
    body: ValueRef, // cons list
    env: EnvRef, // captured env (lexical scope)
    // TODO: Add table for function argument to make beta reduction faster
};

/// empty is a ConsCell such that both its car and cdr are itself.
pub fn empty() ValueRef {
    if (empty_opt) |e| return e;
    var e = alloc.create(Value) catch @panic("failed to alloc nil");
    empty_cons_opt = new(Cons, Cons{ .car = e, .cdr = e }) catch unreachable;
    e.* = Value{ .cons = empty_cons_opt.? };
    empty_opt = e;
    return empty_opt.?;
}

fn emptyCons() *const Cons {
    _ = empty();
    return empty_cons_opt.?;
}

var empty_opt: ?*Value = null;
var empty_cons_opt: ?*Cons = null;

pub fn quote() ValueRef {
    initSpecialSymbol("quote", &quote_opt);
    return quote_opt.?;
}

var quote_opt: ?*Value = null;

/// #f.
/// The only falsy value.
pub fn f() ValueRef {
    initSpecialSymbol("#f", &f_opt);
    return f_opt.?;
}

var f_opt: ?*Value = null;

/// #t.
/// #t is just a non-special symbol in Scheme but useful to implement interpreter.
pub fn t() ValueRef {
    initSpecialSymbol("#t", &t_opt);
    return t_opt.?;
}

var t_opt: ?*Value = null;

fn initSpecialSymbol(sym: []const u8, dst: *?*Value) void {
    if (dst.* != null) return;
    var ptr = alloc.create(Value) catch unreachable;
    ptr.* = Value{ .symbol = S.getOrRegister(sym) catch unreachable };
    dst.* = ptr;
}

/// Convert sequence of cons cell like (foo bar buz) to slice.
pub fn toSlice(head: ValueRef, to: []ValueRef) usize {
    var i: usize = 0;
    var h = head;
    //  std.log.debug("aaaaa", .{});
    while (h != empty()) {
        // std.log.debug("list: {s}", .{toString(h) catch unreachable});
        // std.log.debug("car: {s}", .{toString(h.cons.car) catch unreachable});
        // std.log.debug("cdr: {s}", .{toString(h.cons.cdr) catch unreachable});
        std.debug.assert(@as(ValueTag, h.*) == ValueTag.cons); // is cons?
        to[i] = h.cons.car;
        i += 1;
        h = h.cons.cdr;
    }
    return i;
}

/// The "deep equal" function for values.
/// Equality of function values are only based on the names.
pub fn deepEql(x: ValueRef, y: ValueRef) bool {
    if (x == empty() or y == empty()) return x == y;
    if (@as(ValueTag, x.*) != @as(ValueTag, y.*)) return false;
    switch (x.*) {
        Value.number => |x_| return x_ == y.number,
        Value.symbol => |x_| return x_ == y.symbol,
        Value.b_func => |x_| return x_ == y.b_func,
        Value.b_spf => |x_| return x_ == y.b_spf,
        Value.cons => |x_| return deepEql(x_.car, y.cons.car) and deepEql(x_.cdr, y.cons.cdr),
        Value.function => |x_| return x_.name == y.function.name, // just comparing name
    }
}

pub fn toString(cell: ValueRef) ![]const u8 {
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();
    try toStringInner(cell, &buf);
    return try buf.toOwnedSlice();
}

fn toStringInner(cell: ValueRef, builder: *std.ArrayList(u8)) anyerror!void {
    if (cell == empty()) {
        try builder.appendSlice("()");
        return;
    }
    switch (cell.*) {
        Value.cons => |c| {
            try builder.append('(');
            try consToString(c, builder);
            try builder.append(')');
        },
        Value.number => |num| {
            var buffer: [30]u8 = undefined;
            const str = std.fmt.bufPrint(
                buffer[0..],
                "{}",
                .{num},
            ) catch @panic("too large integer");
            try builder.appendSlice(str);
        },
        Value.symbol => |sym| try builder.appendSlice(S.getName(sym).?),
        Value.function => |func| {
            if (func.name) |n| {
                try builder.appendSlice("<function:");
                try builder.appendSlice(S.getName(n).?);
                try builder.appendSlice(">");
            } else {
                try builder.appendSlice("<lambda>");
            }
        },
        Value.b_func => try builder.appendSlice("<builtin function>"),
        Value.b_spf => try builder.appendSlice("<builtin special form>"),
    }
}

fn consToString(x: *const Cons, builder: *std.ArrayList(u8)) !void {
    switch (x.cdr.*) {
        Value.cons => |next| {
            // List
            try toStringInner(x.car, builder);
            if (next == emptyCons()) return;
            try builder.append(' ');
            try consToString(next, builder);
        },
        else => {
            // Dotted pair
            try toStringInner(x.car, builder);
            try builder.appendSlice(" . ");
            try toStringInner(x.cdr, builder);
        },
    }
}

// pub fn panicAt(tok: T, message: []const u8) void {
//     const stderr = std.io.getStdErr().writer();
//     nosuspend {
//         stderr.print("error: {s}\n", .{message});
//         stderr.print("error: {s}\n", .{message});
//         for (0..tok.index + 7) |_| stderr.print(" ", .{});
//         stderr.print("^\n");
//     }
//     unreachable;
// }

pub fn _car(x: ValueRef) ValueRef {
    return x.cons.car;
}

pub fn _cdr(x: ValueRef) ValueRef {
    return x.cons.cdr;
}

pub fn _cddr(x: ValueRef) ValueRef {
    return _cdr(x).cons.cdr;
}

pub fn _cdddr(x: ValueRef) ValueRef {
    return _cddr(x).cons.cdr;
}

pub fn _cadr(x: ValueRef) ValueRef {
    return _cdr(x).cons.car;
}

pub fn _caddr(x: ValueRef) ValueRef {
    return _cddr(x).cons.car;
}

pub fn _cadddr(x: ValueRef) ValueRef {
    return _cdddr(x).cons.car;
}
