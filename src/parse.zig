const std = @import("std");
const assert = std.debug.assert;

const T = @import("tok.zig").Token;
const S = @import("symbol.zig");
const C = @import("common.zig");
const Mem = @import("mem.zig");

const TokenKind = @import("tok.zig").TokenKind;
const Value = C.Value;
const ValueRef = C.ValueRef;

// parse returns list of s-expr.
pub fn parse(tokens: []const T) ![]ValueRef {
    var ret = std.ArrayList(ValueRef).init(C.alloc);
    var rest = tokens;
    while (rest.len != 0) {
        const value, rest = try parseSExpr(rest);
        try ret.append(value);
    }
    return ret.toOwnedSlice();
}

/// Parse s-expression based on BFN below.
/// <sexpr>  ::= <atom>
///            | '(' <sexpr>* ')'
///            | '(' <sexpr>+ '.' <sexpr> ')'
///            | <quote> <sexpr>
fn parseSExpr(tokens: []const T) anyerror!struct { ValueRef, []const T } {
    if (tokens.len == 0)
        @panic("no tokens");

    const head = tokens[0];
    const tail = tokens[1..];
    switch (head.kind) {
        TokenKind.left => {
            assert(tail.len != 0);
            const value, var rest = try parseList(tail);
            rest = rest[1..]; // consume ")"
            return .{ value, rest };
        },
        TokenKind.quote => {
            // <quote> <sexpr> => (quote <sexpr>)
            const value, const rest = try parseSExpr(tail);
            const q = try Mem.newValue(Value{ .symbol = try S.getOrRegister("quote") });
            const quote = try C.newCons(q, try C.newCons(value, C.empty()));
            return .{ quote, rest };
        },
        TokenKind.int => |int| {
            const atom = try Mem.newValue(Value{ .number = int });
            return .{ atom, tokens[1..] };
        },
        TokenKind.symbol => |symbol| {
            const atom = try Mem.newValue(Value{ .symbol = symbol });
            return .{ atom, tokens[1..] };
        },
        TokenKind.right, TokenKind.dot => unreachable,
        TokenKind.f => return .{ C.f(), tokens[1..] },
    }
}

// Parse sequence of s-expression or dotted pair
// <S-expr>* | <S-expr>+ '.' <S-expr>
fn parseList(tokens: []const T) anyerror!struct { ValueRef, []const T } {
    if (tokens.len == 0) return .{ C.empty(), tokens };

    switch (tokens[0].kind) {
        TokenKind.right => return .{ C.empty(), tokens },
        else => {
            // Parse first S-expr
            const car, var rest = try parseSExpr(tokens);
            if (rest.len > 0 and rest[0].kind == TokenKind.dot) {
                const cdr, rest = try parseSExpr(rest[1..]);
                const ret = try C.newCons(car, cdr);
                return .{ ret, rest };
            }
            // Parse following S-exprs
            const cdr, rest = try parseList(rest);
            // Meld the results of them
            const ret = try C.newCons(car, cdr);
            return .{ ret, rest };
        },
    }
}
