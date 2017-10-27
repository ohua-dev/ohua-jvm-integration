package ohua.alang;


import ohua.types.*;


public abstract class ResolvedSymbol {
    ResolvedSymbol() {}
    public static final class Local extends ResolvedSymbol {
        public final Binding binding;
        public Local(final Binding binding) {
            this.binding = binding;
        }
    }
    public static final class Sf extends ResolvedSymbol {
        public final String fnName;
        public final Integer fnId;
        public Sf(final String fnName) {
            this.fnName = fnName;
            this.fnId = null;
        }
        public Sf(final String fnName, final Integer fnId) {
            this.fnName = fnName;
            this.fnId = fnId;
        }
    }
    public static final class Env extends ResolvedSymbol {
        public final Integer id;
        public Env(final Integer id) {
            this.id = id;
        }
    }
}
