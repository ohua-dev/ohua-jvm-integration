package ohua.alang;

import ohua.types.*;


/**
 * Java version of the "normal" version of alang expressions.
 */
public abstract class Expr {
    public final static class Let extends Expr {
        public final Assignment assignment;
        public final Expr value;
        public final Expr body;

        public Let(final Assignment assignment, final Expr value, final Expr body) {
            this.assignment = assignment;
            this.value = value;
            this.body = body;
        }
    }
    public final static class Lambda extends Expr {
        public final Assignment assignment;
        public final Expr body;

        public Lambda(final Assignment assignment, final Expr body) {
            this.assignment = assignment;
            this.body = body;
        }
    }
    public final static class Apply extends Expr {
        public final Expr function;
        public final Expr argument;

        public Apply(final Expr function, final Expr argument) {
            this.function = function;
            this.argument = argument;
        }
    }
    public final static class Var extends Expr {
        public final Object value;

        public Var(final Object value) {
            this.value = value;
        }
    }
}
