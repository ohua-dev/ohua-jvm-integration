package ohua;

import ohua.types.FnName;


public interface Linker {
    /**
     * Resolve a stateful function.
     * Returns the fully qualified version of the potentially unqualified
     * input name or null if no stateful function with this name is in scope.
     */
    String resolve(String name);
}
