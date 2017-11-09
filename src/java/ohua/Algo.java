package ohua;

import ohua.alang.*;
import ohua.util.Lazy;

/**
 * Captures the copiled alang code for a compiled algo
 * as well as an array of objects that represent any constant input values.
 */
public final class Algo {
    public final Expr code;
    public final Lazy[] envExprs; // perhaps in tha future this should be somthing actually immutable?

    public Algo(final Expr code, final Lazy[] envExprs) {
        this.code = code;
        this.envExprs = envExprs;
    }
}
