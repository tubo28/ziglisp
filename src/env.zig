const std = @import("std");

const S = @import("symbol.zig");
const C = @import("common.zig");

const ValueRef = C.ValueRef;
const Value = C.Value;
const SymbolID = S.ID;

const Map = struct {
    names: ValueRef,
    values: ValueRef,

    const Self = @This();

    fn put(self: *Self, k: SymbolID, v: ValueRef) !bool {
        var n = self.names;
        while (n != C.empty()) {
            if (C._car(n).symbol == k) return false;
            n = C._cdr(n);
        }
        self.names = try C.newCons(try C.new(Value, Value{ .symbol = k }), self.names);
        self.values = try C.newCons(v, self.values);
        return true;
    }

    fn get(self: *const Self, k: SymbolID) ?ValueRef {
        var n = self.names;
        var v = self.values;
        while (n != C.empty()) {
            if (C._car(n).symbol == k) return C._car(v);
            n = C._cdr(n);
            v = C._cdr(v);
        }
        return null;
    }
};

pub const Env = struct {
    map: *const Map,
    caller: ?*const Env,
    global: *Map,

    const Self = @This();
    pub const Ref = *const Self;

    pub fn new(names: ValueRef, values: ValueRef) !Env.Ref {
        std.debug.assert(C.listLength(names) == C.listLength(values));
        var ret = try C.new(Self, undefined);
        const map = try C.new(Map, Map{
            .names = names,
            .values = values,
        });
        ret.* = Env{ .map = map, .caller = null, .global = map };
        return ret;
    }

    pub fn globalDef(self: *const Self, k: SymbolID, v: ValueRef) !void {
        if (!try self.global.put(k, v)) {
            std.log.err("redefine of {s}", .{S.getName(k).?});
            unreachable;
        }
    }

    pub fn fork(self: *const Self, names: ValueRef, values: ValueRef, len: usize) !Env.Ref {
        std.debug.assert(C.listLength(names) == len);
        std.debug.assert(C.listLength(values) == len);

        const map = try C.new(Map, Map{
            .names = names,
            .values = values,
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
