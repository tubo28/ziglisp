const std = @import("std");

const C = @import("common.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const alloc = arena.allocator();

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
