const std = @import("std");
const common = @import("common.zig");

pub const ID = u32;

var sid: ID = undefined;
var to_id: std.StringHashMap(ID) = undefined;
var to_str: std.AutoHashMap(ID, []const u8) = undefined;

pub fn init() !void {
    sid = 1;
    to_id = std.StringHashMap(ID).init(common.alloc);
    to_str = std.AutoHashMap(ID, []const u8).init(common.alloc);
    try registerMany(preset_names, preset_mask);
}

pub fn registerMany(names: anytype, begin: ID) !void {
    for (names, begin..) |s, i| {
        try to_id.put(s, @intCast(i));
        try to_str.put(@intCast(i), s);
    }
}

// Find the symbol ID or register if new.
// The only way to register a symbol.
// TODO: Reduce usage
pub fn getOrRegister(s: []const u8) !ID {
    std.debug.assert(sid != 0);

    sid += 1;
    if (to_id.get(s)) |id| return id;
    try to_id.put(s, sid);
    try to_str.put(sid, s);
    return sid;
}

pub fn getName(s: ID) ?[]const u8 {
    return to_str.get(s);
}

const preset_mask: ID = 1 << 31;
const preset_names = [_][]const u8{ "#f", "#t" };
