const std = @import("std");
pub const assert = std.debug.assert;

const Token = @import("tokenize.zig").Token;
const TokenKind = @import("tokenize.zig").TokenKind;

const common = @import("common.zig");
const Value = common.Value;
const ValueRef = common.ValueRef;
const nil = common.nil;

// parse returns list of s-expr.
pub fn parse(tokens: []const Token) []ValueRef {
    var ret = std.ArrayList(ValueRef).init(common.alloc);
    var rest = tokens;
    while (rest.len != 0) {
        const value, rest = parseSExpr(rest);
        ret.append(value) catch unreachable;
    }
    return ret.toOwnedSlice() catch unreachable;
}

/// Parse s-expression based on BFN below.
/// <sexpr>  ::= <atom>
///            | '(' <sexpr>* ')'
///            | <quote> <sexpr>
fn parseSExpr(tokens: []const Token) struct { ValueRef, []const Token } {
    if (tokens.len == 0)
        @panic("no tokens");

    const head = tokens[0];
    const tail = tokens[1..];
    switch (head.kind) {
        TokenKind.left => {
            assert(tail.len != 0);
            const value, var rest = parseList(tail);
            rest = rest[1..]; // consume ")"
            return .{ value, rest };
        },
        TokenKind.quote => {
            // <quote> <sexpr> => (quote <sexpr>)
            const value, const rest = parseSExpr(tail);
            const quote = common.newSymbolValue("quote");
            return .{
                common.newConsValue(quote, common.newConsValue(value, nil())),
                rest,
            };
        },
        TokenKind.int => |int| {
            const atom = common.newNumberValue(int);
            return .{ atom, tokens[1..] };
        },
        TokenKind.symbol => |symbol| {
            const atom = common.newSymbolValue(symbol);
            return .{ atom, tokens[1..] };
        },
        TokenKind.right => @panic("unbalanced parens"),
        TokenKind.nil => return .{ nil(), tokens[1..] },
    }
}

// Parse sequence of s-expression based on BFN below.
// <S-expr>*
fn parseList(tokens: []const Token) struct { ValueRef, []const Token } {
    if (tokens.len == 0) return .{ nil(), tokens };

    switch (tokens[0].kind) {
        TokenKind.right => return .{ nil(), tokens },
        else => {
            // Parse first S-expr
            const car, var rest = parseSExpr(tokens);
            // Parse following S-exprs
            const cdr, rest = parseList(rest);
            // Meld the results of them
            const ret = common.newConsValue(car, cdr);
            return .{ ret, rest };
        },
    }
}
