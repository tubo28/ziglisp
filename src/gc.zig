const std = @import("std");

const C = @import("common.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const alloc = arena.allocator();

const Value = C.Value;
const ValueRef = C.ValueRef;

const Set = std.DynamicBitSet;

var len: usize = undefined;
var pool: []Value = undefined;

var used: std.ArrayList(usize) = undefined;
var free: std.ArrayList(usize) = undefined;

const AllocationError = error{
    OutOfMemory,
};

fn new() AllocationError!*Value {
    if (used.items.len == len) return AllocationError.OutOfMemory;
    const i = free.pop();
    used.appendAssumeCapacity(i);
    return &pool[i];
}

fn free(i: usize) void {
    var bitset = try std.DynamicBitSet.initEmpty(std.testing.allocator, size);
}

fn init(n: usize) !void {
    len = n;
    pool = try alloc.alloc(Value, len);
    used = try std.ArrayList(usize).initCapacity(alloc, len);
    free = try std.ArrayList(usize).initCapacity(alloc, len);
    for (0..len) |i| free.appendAssumeCapacity(i);
}

fn gc(root: ValueRef) !void {
    var marked = Set.init(alloc);
    mark(root, &marked);
}

test "gc/new" {
    try init(2);
    _ = try new();
    _ = try new();
    try std.testing.expectError(AllocationError.OutOfMemory, new());
}

fn mark(root: ValueRef, marked: *Set) void {
    if (marked.get(root)) return;
    marked.put(root, opaque {});
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
        Value.number, Value.symbol, Value.b_func, Value.b_form => {},
    }
}

fn sweep(root: ValueRef) void {
    _ = root;
}
