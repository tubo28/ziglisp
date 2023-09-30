const std = @import("std");
const common = @import("common.zig");

pub const SymbolID = u32;

var sid: SymbolID = undefined;
var to_id: std.StringHashMap(SymbolID) = undefined;
var to_str: std.AutoHashMap(SymbolID, []const u8) = undefined;

pub fn init() !void {
    sid = 1;
    to_id = std.StringHashMap(SymbolID).init(common.alloc);
    to_str = std.AutoHashMap(SymbolID, []const u8).init(common.alloc);
    try registerMany(preset_names, preset_mask);
    try registerMany(builtin_func_names, builtin_func_mask);
}

pub fn registerMany(names: anytype, begin: SymbolID) !void {
    for (names, begin..) |s, i| {
        try to_id.put(s, @intCast(i));
        try to_str.put(@intCast(i), s);
    }
}

// Find the symbol ID or register if new.
// The only way to register a symbol.
// TODO: Reduce usage
pub fn getOrRegister(s: []const u8) !SymbolID {
    std.debug.assert(sid != 0);

    sid += 1;
    if (to_id.get(s)) |id| return id;
    try to_id.put(s, sid);
    try to_str.put(sid, s);
    return sid;
}

pub fn getName(s: SymbolID) ?[]const u8 {
    return to_str.get(s);
}

pub fn isBuiltinFunc(id: SymbolID) bool {
    return id & builtin_func_mask != 0;
}

const preset_mask: SymbolID = 1 << 31;
const preset_names = [_][]const u8{ "#f", "#t" };

const builtin_func_mask: SymbolID = 1 << 29;
const builtin_func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "<=", ">", ">=", "or", "and", "length", "null?", "quotient", "modulo" };
