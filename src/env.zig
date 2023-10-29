const std = @import("std");

const S = @import("symbol.zig");
const C = @import("common.zig");
const M = @import("map.zig");

const ValueRef = C.ValueRef;
const Value = C.Value;
const SymbolID = S.ID;

pub fn resolve(map: ValueRef, k: ValueRef) ?ValueRef {
    return M.get(map, k) orelse M.get(global, k);
}

var global: ValueRef = undefined;

pub fn init() void {
    global = C.empty();
}

pub fn addGlobal(k: ValueRef, v: ValueRef) !void {
    global = try M.addOne(global, k, v);
}

// var c: usize = 0;
// var count = [12]usize{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

// error: { 94297892, 182702108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
// error: { 214127674, 81872326, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
