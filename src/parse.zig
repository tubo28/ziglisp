const std = @import("std");
pub const assert = std.debug.assert;

const Token = @import("tokenize.zig").Token;
const TokenKind = @import("tokenize.zig").TokenKind;

const Symbol = @import("symbol.zig");

const common = @import("common.zig");
const Value = common.Value;
const ValueRef = common.ValueRef;

// parse returns list of s-expr.
pub fn parse(tokens: []const Token) ![]ValueRef {
    var ret = std.ArrayList(ValueRef).init(common.alloc);
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
///            | <quote> <sexpr>
fn parseSExpr(tokens: []const Token) anyerror!struct { ValueRef, []const Token } {
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
            const quote = try common.newSymbolValue(try Symbol.getOrRegister("quote"));
            return .{
                try common.newConsValue(quote, try common.newConsValue(value, common.empty())),
                rest,
            };
        },
        TokenKind.int => |int| {
            const atom = try common.newNumberValue(int);
            return .{ atom, tokens[1..] };
        },
        TokenKind.symbol => |symbol| {
            const atom = try common.newSymbolValue(symbol);
            return .{ atom, tokens[1..] };
        },
        TokenKind.right => @panic("unbalanced parens"),
        TokenKind.f => return .{ common.f(), tokens[1..] },
    }
}

// Parse sequence of s-expression based on BFN below.
// <S-expr>*
fn parseList(tokens: []const Token) anyerror!struct { ValueRef, []const Token } {
    if (tokens.len == 0) return .{ common.empty(), tokens };

    switch (tokens[0].kind) {
        TokenKind.right => return .{ common.empty(), tokens },
        else => {
            // Parse first S-expr
            const car, var rest = try parseSExpr(tokens);
            // Parse following S-exprs
            const cdr, rest = try parseList(rest);
            // Meld the results of them
            const ret = try common.newConsValue(car, cdr);
            return .{ ret, rest };
        },
    }
}
