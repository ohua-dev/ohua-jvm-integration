package ohua.tests;

import ohua.lang.defsfn;

public final class Trace {
    @defsfn
    public <A> A trace(A a) {
        System.out.println(a);
        return a;
    }
}
