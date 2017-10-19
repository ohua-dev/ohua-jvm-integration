package ohua;


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
     * Returns an `Algo` structure capturing the copiled alang code for the algo
     * as well as an array of objects that represent any constant input values.
     * 
     * If no algo was found returns `null`.
     */
    Algo resolveAlgo(String name);
}
