const std = @import("std");

const C = @import("common.zig");

pub const ID = u32;

var sid: ID = 0;
var to_id: std.StringHashMap(ID) = undefined;
var to_str: std.AutoHashMap(ID, []const u8) = undefined;

pub fn init() !void {
    if (sid != 0) {
        to_id.clearAndFree();
        to_str.clearAndFree();
    }
    sid = 1;
    to_id = std.StringHashMap(ID).init(C.alloc);
    to_str = std.AutoHashMap(ID, []const u8).init(C.alloc);
}

// Find the symbol ID or register if new.
// The only way to register a symbol.
// TODO: Reduce usage
pub fn getOrRegister(s: []const u8) !ID {
    std.debug.assert(sid != 0);
    if (to_id.get(s)) |id| return id;
    sid += 1;

    std.log.debug("register symbol {}: {s}", .{ sid, s });
    try to_id.put(s, sid);
    try to_str.put(sid, s);
    return sid;
}

pub fn registerUnsafe(s: []const u8, id: ID) !void {
    std.debug.assert(to_id.get(s) == null);
    std.debug.assert(to_str.get(id) == null);
    try to_id.put(s, id);
    try to_str.put(id, s);
}

pub fn getName(s: ID) ?[]const u8 {
    return to_str.get(s);
}
