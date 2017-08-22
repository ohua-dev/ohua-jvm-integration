package ohua.util;


public abstract class Source {
    Source() {}

    public static final class Local extends Source {
        public final Target target;

        public Local(final Target target) {
            this.target = target;
        }
    }

    public static final class Env extends Source {
        public final int hostExpr;

        public Env(final int hostExpr) {
            this.hostExpr = hostExpr;
        }
    }
}
