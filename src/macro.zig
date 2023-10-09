const std = @import("std");

const T = @import("tokenize.zig").Token;
const S = @import("symbol.zig");
const C = @import("common.zig");

const Value = C.Value;
const ValueRef = C.ValueRef;

// (define-syntax id expr)
// (syntax-rules (literal-id ...)
//   ((id pattern) template) ...)

// (define-syntax if
//   (syntax-rules ()
//     ((_ test then else)
//      (cond (test then)
//            (else else)))))
// (define-syntax incf
//   (syntax-rules ()
//     ((_ x) (begin (set! x (+ x 1)) x))
//     ((_ x i) (begin (set! x (+ x i)) x))))
pub fn preproc(exprs: []ValueRef) ![]ValueRef {
    var ret = std.ArrayList(ValueRef).init(C.alloc);

    var macros = std.AutoHashMap(S.ID, Macro).init(C.alloc);
    // Ignores `define-syntax` on non top level position.
    for (exprs) |expr| {
        if (@as(C.ValueTag, expr.*) == C.ValueTag.cons and
            @as(C.ValueTag, expr.cons.car.*) == C.ValueTag.symbol and
            expr.cons.car.symbol == try S.getOrRegister("define-syntax"))
        {
            const macro = Macro{
                .name = C._cadr(expr).symbol,
                .rules = try parseRules(C._caddr(expr)),
            };
            try macros.put(macro.name, macro);
            continue;
        }

        try ret.append(expr);
    }
    return ret.toOwnedSlice();
}

fn parseRules(expr: ValueRef) ![]Rule {
    const syntax_rules = try S.getOrRegister("syntax-rules");

    std.debug.assert(C._car(expr).symbol == syntax_rules);
    std.debug.assert(C._cadr(expr) == C.empty()); // TODO

    var rules: []ValueRef = make_rules_slice: {
        var buf: [100]ValueRef = undefined;
        const rules_vr = C._cddr(expr);
        const len = C.toSlice(rules_vr, &buf);
        break :make_rules_slice buf[0..len];
    };

    var ret = try C.alloc.alloc(Rule, rules.len);
    for (rules, 0..) |r, i|
        ret[i] = Rule{ .pattern = C._car(r), .template = C._cadr(r) };
    return ret;
}

const Macro = struct {
    name: S.ID,
    rules: []Rule,
};

const Rule = struct {
    pattern: ValueRef, // Consists only of cons or symbol
    template: ValueRef,
};
