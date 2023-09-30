const std = @import("std");
const common = @import("common.zig");

pub const SymbolID = u32;

var sid: SymbolID = 0;
var sym_map: std.AutoHashMap([]const u8, SymbolID) = undefined;
var id_map: std.StringHashMap(SymbolID) = undefined;

pub fn init() !void {
    sid = 0;
    sym_map = std.AutoHashMap([]const u8, SymbolID).init(common.alloc);
    id_map = std.StringHashMap(SymbolID).init(common.alloc);

    for (builtin_func_names, builtin_func_mask..) |s, i| {
        try sym_map.put(s, i);
        try id_map.put(i, s);
    }
    for (special_form_names, special_form_mask..) |s, i| {
        try sym_map.put(s, i);
        try id_map.put(i, s);
    }
}

pub fn getID(s: []const u8) !SymbolID {
    std.debug.assert(sid != 0);
    if (id_map.get(s)) |id| return id;
    sid += 1;
    try id_map.put(s, sid);
    return sid;
}

pub fn getName(s: SymbolID) ?[]const u8 {
    return id_map.get(s);
}

pub fn isBuiltinFunc(id: SymbolID) bool {
    return id & builtin_func_mask == 0;
}

pub fn isSpecialForm(id: SymbolID) bool {
    return id & special_form_mask == 0;
}

const builtin_func_mask: SymbolID = 1 << 31;
const builtin_func_names = [_][]const u8{
    "car",
    "cdr",
    "cons",
    "list",
    "print",
    "+",
    "-",
    "*",
    "=",
    "<",
    "<=",
    ">",
    ">=",
    "or",
    "and",
    "length",
    "null?",
    "quotient",
    "modulo",
};

const special_form_mask: SymbolID = 1 << 30;
const special_form_names = [_][]const u8{
    "quote",
    "begin",
    "define",
    "lambda",
    "if",
    "cond",
    "let",
};

const preset_names = [_][]const u8{ "#f", "#t" };
