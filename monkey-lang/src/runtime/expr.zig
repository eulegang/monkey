const std = @import("std");
const value = @import("./value.zig");
const expr = @import("../parser/expr.zig");
const stmt = @import("../parser/stmt.zig");

pub fn eval_expr(e: *expr.Expr) value.Value {
    switch (e.*) {
        .number => |n| {
            return value.Value{
                .number = value.Number{ .value = @bitCast(i64, n.value) },
            };
        },

        .boolean => |b| {
            return value.Value{ .boolean = value.Boolean{ .value = b.value } };
        },

        .prefix => |p| return eval_prefix(p),
        .infix => |i| return eval_infix(i),
        .cond => |c| return eval_cond(c),

        else => {
            unreachable;
        },
    }
}

fn eval_prefix(e: expr.Prefix) value.Value {
    switch (e.op) {
        .Not => {
            return value.Value{
                .boolean = value.Boolean{
                    .value = !eval_expr(e.expr).truthy(),
                },
            };
        },

        .Neg => {
            const val = eval_expr(e.expr);

            switch (val) {
                .number => |n| {
                    return value.Value{
                        .number = value.Number{ .value = -n.value },
                    };
                },

                else => {
                    return value.Value{
                        .nil = value.Nil{},
                    };
                },
            }
        },
    }
}

fn eval_infix(e: expr.Infix) value.Value {
    switch (e.op) {
        .Eq => {
            const l = eval_expr(e.lhs);
            const r = eval_expr(e.rhs);

            return value.Value{
                .boolean = value.Boolean{ .value = l.eql(r) },
            };
        },

        .Neq => {
            const l = eval_expr(e.lhs);
            const r = eval_expr(e.rhs);

            return value.Value{
                .boolean = value.Boolean{ .value = !l.eql(r) },
            };
        },

        .Lt => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .boolean = value.Boolean{ .value = lhs < rhs } };
        },

        .Gt => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .boolean = value.Boolean{ .value = lhs > rhs } };
        },

        .Le => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .boolean = value.Boolean{ .value = lhs <= rhs } };
        },

        .Ge => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .boolean = value.Boolean{ .value = lhs >= rhs } };
        },

        .Add => { // cheeky reference to a homomorphic type category
            const le = eval_expr(e.lhs);
            const re = eval_expr(e.rhs);

            if (le.type_tag() != re.type_tag()) {
                return value.Value{ .nil = value.Nil{} };
            }

            switch (le) {
                .number => |l| {
                    const r = re.number;

                    return value.Value{ .number = value.Number{ .value = l.value + r.value } };
                },

                .boolean => |l| {
                    const r = re.boolean;

                    return value.Value{ .boolean = value.Boolean{ .value = l.value or r.value } };
                },

                .nil => {
                    return value.Value{ .nil = value.Nil{} };
                },
            }
        },

        .Sub => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .number = value.Number{ .value = lhs - rhs } };
        },

        .Mult => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .number = value.Number{ .value = lhs * rhs } };
        },

        .Div => {
            const l = eval_expr(e.lhs);
            if (l.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const r = eval_expr(e.rhs);
            if (r.type_tag() != value.Type.number) {
                return value.Value{ .nil = value.Nil{} };
            }

            const lhs = l.number.value;
            const rhs = r.number.value;

            return value.Value{ .number = value.Number{ .value = @divTrunc(lhs, rhs) } };
        },
    }
}

fn eval_cond(e: expr.Cond) value.Value {
    const cond = eval_expr(e.cond);

    if (cond.truthy()) {
        return eval_blk(e.cons);
    } else {
        if (e.alt) |alt| {
            return eval_blk(alt);
        } else {
            return value.Value{ .nil = value.Nil{} };
        }
    }
}

fn eval_blk(e: *expr.Block) value.Value {
    var val: value.Value = value.Value{ .nil = value.Nil{} };
    for (e.stmts.items) |item| {
        val = eval_stmt(item);
    }

    return val;
}

fn eval_stmt(s: stmt.Stmt) value.Value {
    _ = s;
    unreachable;
}
