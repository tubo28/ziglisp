const std = @import("std");

const S = @import("symbol.zig");
const C = @import("common.zig");

const ValueRef = C.ValueRef;
const SymbolID = S.ID;

const Map = std.AutoArrayHashMap(SymbolID, ValueRef);

fn newMap() !*Map {
    const ret = try C.alloc.create(Map);
    ret.* = Map.init(C.alloc);
    return ret;
}

pub const Env = struct {
    map: *const Map,
    caller: ?*const Env,
    global: *Map,

    const Self = @This();
    pub const Ref = *const Self;

    pub fn new() !Env.Ref {
        var ret = try C.alloc.create(Self);
        const map = try newMap();
        ret.* = Env{ .map = map, .caller = null, .global = map };
        return ret;
    }

    pub fn globalDef(self: *const Self, k: SymbolID, v: ValueRef) !void {
        const result = try self.global.getOrPut(k);
        if (result.found_existing) {
            std.log.err("redefine of {s}", .{S.getName(k).?});
            unreachable;
        }
        result.value_ptr.* = v;
    }

    pub fn fork(self: *const Self, kvs: []struct { SymbolID, ValueRef }) !Env.Ref {
        var m = try newMap();
        try m.ensureTotalCapacity(kvs.len);
        for (kvs) |kv| {
            try m.put(kv[0], kv[1]);
        }
        var ret = try C.alloc.create(Self);
        ret.* = Env{ .map = m, .caller = self, .global = self.global };
        return ret;
    }

    pub fn get(self: *const Self, k: SymbolID) ?ValueRef {
        const ret = self.getImpl(k);
        if (ret) |r| @constCast(self.map).put(k, r) catch unreachable;
        // count[ret[1]] += 1;
        // c += 1;
        // if (c % 1_000_000 == 0) {
        //     std.log.err("{any}", .{count});
        // }
        return ret;
    }

    fn getImpl(self: *const Self, k: SymbolID) ?ValueRef {
        if (self.map.get(k)) |v| return v;
        if (self.caller) |c| return c.getImpl(k);
        return self.global.get(k);
    }
};

// var c: usize = 0;
// var count = [12]usize{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

// error: { 94297892, 182702108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
// error: { 214127674, 81872326, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
