package ohua.graph;


import ohua.types.*;


public final class Operator {

    public final int id;
    public final FnName type;


    public Operator(final int id, final FnName type) {
        this.id = id;
        this.type = type;
    }
}
