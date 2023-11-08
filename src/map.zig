const std = @import("std");

const C = @import("common.zig");

const ValueRef = C.ValueRef;
const newCons = C.newCons;
const empty = C.empty;

pub fn addOne(map: ValueRef, symbol: ValueRef, value: ValueRef) !ValueRef {
    const cons = try newCons(symbol, value);
    return try newCons(cons, map);
}

pub fn addAll(map: ValueRef, symbols: ValueRef, values: ValueRef) !ValueRef {
    if (symbols.isEmpty() and values.isEmpty()) return map;
    const k = symbols.car();
    const v = values.car();
    return try addAll(try addOne(map, k, v), symbols.cdr(), values.cdr());
}

pub fn get(map: ValueRef, k: ValueRef) ?ValueRef {
    if (map.isEmpty()) return null;
    const pair = map.car();
    if (pair.car().symbol() == k.symbol()) return pair.cdr();
    return get(map.cdr(), k);
}
