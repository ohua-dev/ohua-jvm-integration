package ohua.util;

import clojure.lang.Symbol;
import clojure.java.api.Clojure;
import clojure.lang.IFn;

// FIXME it is yet unclear to me whether we really need this class. currently Compat.hs

/**
 * Created by sertel on 8/22/17.
 */
public class Eta {
  private static final IFn mkSym = Clojure.var("clojure.core", "symbol");
  public static Symbol createSymbol(String namespace, String name) {
    return (Symbol) mkSym.invoke(namespace, name);
  }
  public static Symbol createSymbol(String name) {
    return (Symbol) mkSym.invoke(name);
  }
}
