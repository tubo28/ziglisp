const std = @import("std");

const S = @import("symbol.zig");
const C = @import("common.zig");

const ValueRef = C.ValueRef;
const SymbolID = S.ID;

const Map = struct {
    names: []SymbolID,
    values: []ValueRef,
    len: usize,
    cap: usize,

    const Self = @This();

    fn put(self: *Self, k: SymbolID, v: ValueRef) bool {
        for (self.names) |n| if (n == k) return false;

        std.debug.assert(self.len != self.cap); // no capacity
        self.names[self.len] = k;
        self.values[self.len] = v;
        self.len += 1;
        return true;
    }

    fn get(self: *const Self, k: SymbolID) ?ValueRef {
        for (self.names, self.values) |n, v| if (n == k) return v;
        return null;
    }
};

pub const Env = struct {
    map: *const Map,
    caller: ?*const Env,
    global: *Map,

    const Self = @This();
    pub const Ref = *const Self;

    pub fn new(names: []SymbolID, values: []ValueRef, len: usize) !Env.Ref {
        std.debug.assert(names.len == values.len);
        const cap = names.len;
        var ret = try C.new(Self, undefined);
        const map = try C.new(Map, Map{
            .names = names,
            .values = values,
            .len = len,
            .cap = cap,
        });
        ret.* = Env{ .map = map, .caller = null, .global = map };
        return ret;
    }

    pub fn globalDef(self: *const Self, k: SymbolID, v: ValueRef) void {
        if (!self.global.put(k, v)) {
            std.log.err("redefine of {s}", .{S.getName(k).?});
            unreachable;
        }
    }

    pub fn fork(self: *const Self, names: ValueRef, values: ValueRef, len: usize) !Env.Ref {
        std.debug.assert(C.listLength(names) == len);
        std.debug.assert(C.listLength(values) == len);
        var names_var_slice: [100]ValueRef = undefined;
        const nl = C.flatten(names, &names_var_slice);
        std.debug.assert(nl == len);
        var names_slice = try C.alloc.alloc(SymbolID, len);
        for (0..len) |i| names_slice[i] = names_var_slice[i].symbol;

        var values_slice = try C.alloc.alloc(ValueRef, len);
        const nv = C.flatten(values, values_slice);
        std.debug.assert(nv == len);

        const map = try C.new(Map, Map{
            .names = names_slice,
            .values = values_slice,
            .len = len,
            .cap = len,
        });
        return try C.new(Self, Env{ .map = map, .caller = self, .global = self.global });
    }

    pub fn get(self: *const Self, k: SymbolID) ?ValueRef {
        if (self.map.get(k)) |v| return v;
        if (self.caller) |c| return c.get(k);
        return self.global.get(k);
    }
};

// var c: usize = 0;
// var count = [12]usize{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

// error: { 94297892, 182702108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
// error: { 214127674, 81872326, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
