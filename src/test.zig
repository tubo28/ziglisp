const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

test "head tail" {
    const expect = std.testing.expect;
    _ = expect;
    const a = [_]i32{ 0, 1, 2 };
    print("{any}\n", .{a[1..].*});
    print("{any}\n", .{a[3..].*});
    print("{any}\n", .{a[-1].*});
}
