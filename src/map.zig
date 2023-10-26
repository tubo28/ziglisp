const std = @import("std");

const C = @import("common.zig");

const ValueRef = C.ValueRef;
const newCons = C.newCons;
const _car = C._car;
const _cdr = C._cdr;
const empty = C.empty;

pub fn add(map: ValueRef, k: ValueRef, v: ValueRef) !ValueRef {
    const cons = try newCons(k, v);
    return try newCons(cons, map);
}

pub fn addAll(map: ValueRef, ks: ValueRef, vs: ValueRef) !ValueRef {
    if (ks == empty() and vs == empty()) return map;
    const k = _car(ks);
    const v = _car(vs);
    return try addAll(try add(map, k, v), _cdr(ks), _cdr(vs));
}

pub fn get(map: ValueRef, k: ValueRef) ?ValueRef {
    if (map == empty()) return null;
    const pair = _car(map);
    if (_car(pair).symbol == k.symbol) return _cdr(pair);
    return get(_cdr(map), k);
}
