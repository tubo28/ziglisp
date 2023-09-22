const std = @import("std");
const common = @import("common.zig");
const alloc = common.alloc;

pub const Token = union(enum) {
    int: i64,
    symbol: []const u8,
    left, // (
    right, // )
    quote, // '
    nil, // nil
};

fn isSymbolChar(c: u8) bool {
    return !std.ascii.isWhitespace(c) and c != ')' and c != '(' and c != '\'';
}

pub fn tokenize(code: []const u8) []const Token {
    var toks = std.ArrayList(Token).init(alloc); // defer deinit?

    var i: usize = 0;
    while (i < code.len) {
        const ascii = std.ascii;
        if (ascii.isWhitespace(code[i])) {
            i += 1;
            continue;
        }

        if (code[i] == ';') {
            i += 1;
            while (i < code.len and code[i] != '\n') i += 1;
            continue;
        }

        if (code[i] == '(') {
            toks.append(Token{ .left = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (code[i] == ')') {
            toks.append(Token{ .right = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (code[i] == '\'') {
            toks.append(Token{ .quote = {} }) catch unreachable;
            i += 1;
            continue;
        }

        if (ascii.isDigit(code[i])) {
            var begin = i;
            while (i < code.len and ascii.isDigit(code[i]))
                i += 1;
            const val = std.fmt.parseInt(i64, code[begin..i], 10) catch unreachable;
            toks.append(Token{ .int = val }) catch unreachable;
            continue;
        }

        // All other chars are parts of as symbol.
        {
            var begin = i;
            while (i < code.len and isSymbolChar(code[i]))
                i += 1;
            const sym = code[begin..i];
            // special symbol
            if (std.mem.eql(u8, sym, "nil")) {
                toks.append(Token{ .nil = {} }) catch unreachable;
                continue;
            }
            toks.append(Token{ .symbol = sym }) catch unreachable;
            continue;
        }
    }
    return toks.items;
}
