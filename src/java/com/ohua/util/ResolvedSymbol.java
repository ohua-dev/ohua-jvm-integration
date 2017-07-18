package com.ohua.util;

public abstract class ResolvedSymbol {
    public static final class Local extends ResolvedSymbol {
        public final Binding binding;
        public Local(final Binding binding) {
            this.binding = binding;
        }
    }
    public static final class Sf extends ResolvedSymbol {
        public final FnName fnName;
        public final Integer fnId;
        public Sf(final FnName fnName) {
            this.fnName = fnName;
            this.fnId = null;
        }
        public Sf(final FnName fnName, final Integer fnId) {
            this.fnName = fnName;
            this.fnId = fnId;
        }
    }
    public static final class Algo extends ResolvedSymbol {
        public final FnName algoName;
        public Algo(final FnName algoName) {
            this.algoName = algoName;
        }
    }
    public static final class Env extends ResolvedSymbol {
        public final int id;
        public Env(final int id) {
            this.id = id;
        }
    }
}
