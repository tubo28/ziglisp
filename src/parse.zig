const std = @import("std");
pub const assert = std.debug.assert;

const Token = @import("tokenize.zig").Token;

const common = @import("common.zig");
const Value = common.Value;
const nil = common.nil;

pub const ParseResult = struct {
    value: *const Value,
    rest: []const Token,
};

/// Parse s-expression based on following BFN
/// <sexpr>  ::= <atom>
///            | '(' <sexpr>* ')'
///            | <quote> <sexpr>
pub fn parse(tokens: []const Token) ParseResult {
    if (tokens.len == 0) {
        // Should panic?
        @panic("no tokens");
    }

    const head = tokens[0];
    const tail = tokens[1..];

    switch (head) {
        Token.left => {
            assert(tail.len != 0);
            var ret = parseList(tail);
            ret.rest = ret.rest[1..]; // consume ")"
            return ret;
        },
        Token.quote => {
            // <quote> <sexpr> => (quote <sexpr>)
            const listResult = parse(tail);
            const quote = common.newAtomValue([]const u8, "quote");
            return ParseResult{
                .value = common.newConsValue(quote, common.newConsValue(listResult.value, nil())),
                .rest = listResult.rest,
            };
        },
        Token.int => |int| {
            const atom = common.newAtomValue(i64, int);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.symbol => |symbol| {
            const atom = common.newAtomValue([]const u8, symbol);
            return ParseResult{ .value = atom, .rest = tokens[1..] };
        },
        Token.right => @panic("unbalanced parens"),
        Token.nil => return ParseResult{ .value = nil(), .rest = tokens[1..] },
    }
}

// Parse s-expression based on following BFN
// <S-expr>*
fn parseList(tokens: []const Token) ParseResult {
    if (tokens.len == 0) {
        return ParseResult{ .value = nil(), .rest = tokens };
    }

    switch (tokens[0]) {
        Token.right => return ParseResult{ .value = nil(), .rest = tokens },
        else => {
            // Parse first S-expr
            const car = parse(tokens);
            // Parse following S-exprs
            const cdr = parseList(car.rest);
            // Meld the results of them
            const ret = common.newConsValue(car.value, cdr.value);
            return ParseResult{ .value = ret, .rest = cdr.rest };
        },
    }
}
