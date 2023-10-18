const std = @import("std");

const S = @import("symbol.zig");
const C = @import("common.zig");

const ValueRef = C.ValueRef;
const SymbolID = S.ID;

const Map = std.AutoHashMap(SymbolID, ValueRef);

fn newMap() !*Map {
    const ret = try C.alloc.create(Map);
    ret.* = Map.init(C.alloc);
    return ret;
}

pub const Env = struct {
    map: *const Map,
    parent: ?*const Env,
    global: *Map,

    const Self = @This();
    pub const Ref = *const Self;

    pub fn new() !Env.Ref {
        var ret = try C.alloc.create(Self);
        const map = try newMap();
        ret.* = Env{ .map = map, .parent = null, .global = map };
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

    pub fn overwrite(self: *const Self, kvs: []struct { SymbolID, ValueRef }) !Env.Ref {
        var m = try newMap();
        for (kvs) |kv| try m.put(kv[0], kv[1]);
        var ret = try C.alloc.create(Self);
        ret.* = Env{ .map = m, .parent = self, .global = self.global };
        return ret;
    }

    pub fn get(self: *const Self, k: SymbolID) ?ValueRef {
        const ret = self.getImpl(k);
        if (ret) |r| @constCast(self.map).put(k, r) catch unreachable;
        return ret;
    }

    fn getImpl(self: *const Self, k: SymbolID) ?ValueRef {
        if (self.map.get(k)) |v| return v;
        if (self.parent) |p| return p.get(k);
        return self.global.get(k);
    }
};
