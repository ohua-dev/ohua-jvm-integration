package ohua.graph;

import ohua.util.Tuple;
import java.util.List;

public class GraphFile {
    public final Graph<Integer> graph;
    public final List<Tuple<String[], String>> sfDependencies;
    public final int mainArity;
    public final Target returnArc;

    public GraphFile(final Graph<Integer> graph, final List<Tuple<String[],String>> sfDependencies, int mainArity, final Target returnArc) {
        this.graph = graph;
        this.sfDependencies = sfDependencies;
        this.mainArity = mainArity;
        this.returnArc = returnArc;
    }
}
