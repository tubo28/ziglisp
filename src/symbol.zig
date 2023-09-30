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
    try register(preset_names, preset_mask);
    try register(builtin_func_names, builtin_func_mask);
    try register(special_form_names, special_form_mask);
}

fn register(names: anytype, begin: SymbolID) !void {
    for (names, begin..) |s, i| {
        try to_id.put(s, @intCast(i));
        try to_str.put(@intCast(i), s);
    }
}

// Find the symbol ID or register if new.
// The only way to register a symbol.
// TODO: Reduce usage
pub fn getIDOrNew(s: []const u8) !SymbolID {
    std.debug.assert(sid != 0);

    if (to_id.get(s)) |id| return id;
    try to_id.put(s, sid);
    try to_str.put(sid, s);
    sid += 1;
    return sid;
}

pub fn getName(s: SymbolID) ?[]const u8 {
    return to_str.get(s);
}

pub fn isBuiltinFunc(id: SymbolID) bool {
    return id & builtin_func_mask != 0;
}

pub fn isSpecialForm(id: SymbolID) bool {
    return id & special_form_mask != 0;
}

const preset_mask: SymbolID = 1 << 31;
const preset_names = [_][]const u8{ "#f", "#t" };

const special_form_mask: SymbolID = 1 << 30;
const special_form_names = [_][]const u8{ "quote", "begin", "define", "lambda", "if", "cond", "let" };

const builtin_func_mask: SymbolID = 1 << 29;
const builtin_func_names = [_][]const u8{ "car", "cdr", "cons", "list", "print", "+", "-", "*", "=", "<", "<=", ">", ">=", "or", "and", "length", "null?", "quotient", "modulo" };
