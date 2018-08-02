package ohua;

import ohua.util.Lazy;

/**
 * Support class for the compiler.
 */
public interface Linker {
    /**
     * Resolve a stateful function.
     * Returns the fully qualified version of the potentially unqualified
     * input name or null if no stateful function with this name is in scope.
     */
    String resolve(String name);
    /**
     * Resolve an algo.
     * Returns an `Algo` structure capturing the compiled alang code for the algo
     * as well as an array of objects that represent the constant input values.
     *
     * If no algo was found returns `null`.
     */
    Algo resolveAlgo(String name);
    /**
     * Return a string representation of an environment expression.
     * The default implementation simply calls `toString` on its input.
     */
    default String showExpr(Object expr) {
        return expr.toString();
    }
    default Lazy<Object> eval(Object expr) {
        return Lazy.createRealized(expr);
    }
}
