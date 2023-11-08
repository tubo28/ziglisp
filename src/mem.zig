const std = @import("std");

const C = @import("common.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const alloc = arena.allocator();

const Value = C.Value;
const ValueRef = C.ValueRef;

pub fn newValue(x: Value) !*Value {
    std.log.debug("alloc.create {} bytes for {}.{s}", .{ @sizeOf(Value), Value, @tagName(x) });
    var ret: *Value = try alloc.create(Value);
    ret.* = x;
    return ret;
}
