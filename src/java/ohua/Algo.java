package ohua;


public final class Algo {
    public final Expr code;
    public final Object[] envExprs;

    public Algo(final Expr code, final Object[] envExprs) {
        this.code = code;
        this.envExprs = envExprs;
    }
}
