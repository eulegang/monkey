const std = @import("std");
const Value = @import("./value.zig").Value;
const expr = @import("../parser/expr.zig");
const stmt = @import("../parser/stmt.zig");

pub fn eval_expr(e: *expr.Expr) Value {
    switch (e.*) {
        .number => |n| return Value.from(@bitCast(i64, n.value)),
        .boolean => |b| return Value.from(b.value),

        .prefix => |p| return eval_prefix(p),
        .infix => |i| return eval_infix(i),
        .cond => |c| return eval_cond(c),

        else => {
            unreachable;
        },
    }
}

fn eval_prefix(e: expr.Prefix) Value {
    switch (e.op) {
        .Not => {
            return Value.from(!eval_expr(e.expr).truthy());
        },

        .Neg => {
            const val = eval_expr(e.expr);

            switch (val) {
                .number => |n| return Value.from(-n.value),
                else => return Value.from(void),
            }
        },
    }
}

fn eval_infix(e: expr.Infix) Value {
    switch (e.op) {
        .Eq => {
            const l = eval_expr(e.lhs);
            const r = eval_expr(e.rhs);

            return Value.from(l.eql(r));
        },

        .Neq => {
            const l = eval_expr(e.lhs);
            const r = eval_expr(e.rhs);

            return Value.from(!l.eql(r));
        },

        .Lt => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs < rhs);
        },

        .Gt => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs > rhs);
        },

        .Le => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs <= rhs);
        },

        .Ge => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs >= rhs);
        },

        .Add => { // cheeky reference to a homomorphic type category
            const le = eval_expr(e.lhs);
            const re = eval_expr(e.rhs);

            if (le.type_tag() != re.type_tag()) {
                return Value.from(void);
            }

            switch (le) {
                .number => |l| return Value.from(l.value + re.number.value),
                .boolean => |l| return Value.from(l.value or re.boolean.value),
                .nil => return Value.from(void),
            }
        },

        .Sub => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs - rhs);
        },

        .Mult => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(lhs * rhs);
        },

        .Div => {
            const lhs = eval_expr(e.lhs).integer() orelse return Value.from(void);
            const rhs = eval_expr(e.rhs).integer() orelse return Value.from(void);

            return Value.from(@divTrunc(lhs, rhs));
        },
    }
}

fn eval_cond(e: expr.Cond) Value {
    const cond = eval_expr(e.cond);

    if (cond.truthy()) {
        return eval_blk(e.cons);
    } else {
        if (e.alt) |alt| {
            return eval_blk(alt);
        } else {
            return Value.from(void);
        }
    }
}

fn eval_blk(e: *expr.Block) Value {
    var val: Value = Value.from(void);
    for (e.stmts.items) |item| {
        val = eval_stmt(item);
    }

    return val;
}

fn eval_stmt(s: stmt.Stmt) Value {
    _ = s;
    unreachable;
}
