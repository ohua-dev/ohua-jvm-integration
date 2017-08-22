package ohua.alang;

import ohua.types.*;

public abstract class Assignment {
    public static final class Direct extends Assignment {
        public final Binding binding;

        public Direct(Binding binding) {
            this.binding = binding;
        }
    }

    public static final class Destructure extends Assignment {
        public final Binding[] bindings;

        public Destructure(Binding... bindings) {
            this.bindings = bindings;
        }
    }
}
