const std = @import("std");

const C = @import("common.zig");

const ValueRef = C.ValueRef;
const newCons = C.newCons;
const _car = C._car;
const _cdr = C._cdr;
const empty = C.empty;

pub fn addOne(map: ValueRef, symbol: ValueRef, value: ValueRef) !ValueRef {
    const cons = try newCons(symbol, value);
    return try newCons(cons, map);
}

pub fn addAll(map: ValueRef, symbols: ValueRef, values: ValueRef) !ValueRef {
    if (symbols == empty() and values == empty()) return map;
    const k = _car(symbols);
    const v = _car(values);
    return try addAll(try addOne(map, k, v), _cdr(symbols), _cdr(values));
}

pub fn get(map: ValueRef, k: ValueRef) ?ValueRef {
    if (map == empty()) return null;
    const pair = _car(map);
    if (_car(pair).symbol == k.symbol) return _cdr(pair);
    return get(_cdr(map), k);
}
