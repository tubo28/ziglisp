const std = @import("std");

const C = @import("common.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const Value = C.Value;
const ValueRef = C.ValueRef;

pub fn new(ty: anytype, x: ty) !*ty {
    if (ty == Value) {
        std.log.debug("alloc.create {} bytes for {}.{s}", .{ @sizeOf(ty), ty, @tagName(x) });
    } else {
        std.log.debug("alloc.create {} bytes for {}", .{ @sizeOf(ty), ty });
    }
    var ret: *ty = try alloc.create(ty);
    ret.* = x;
    return ret;
}

const Set = std.AutoHashMap(ValueRef, void);

fn mark(root: ValueRef, marked: *Set) void {
    if (marked.get(root)) return;

    switch (root.*) {
        Value.cons => |cons| {
            mark(cons.car, marked);
            mark(cons.cdr, marked);
        },
        Value.lambda => |lambda| {
            mark(lambda.params, marked);
            mark(lambda.body, marked);
            mark(lambda.closure, marked);
        },
        Value.number, Value.symbol, Value.b_func, Value.b_form => {
            marked.put(root, opaque {});
        },
    }
}

fn sweep(root: ValueRef) void {
    _ = root;
}
