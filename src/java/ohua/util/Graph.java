package ohua.util;


public final class Graph {
    public final Operator[] operators;
    public final Arc[] arcs;

    public Graph(final Operator[] operators, final Arc[] arcs) {
        this.operators = operators;
        this.arcs = arcs;
    }
}
