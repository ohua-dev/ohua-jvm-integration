package ohua.graph;

import ohua.util.Tuple;
import java.util.List;

public class GraphFile {
    public final Graph<Integer> graph;
    public final List<Tuple<String[], String>> sfDependencies;
    public final int mainArity;

    public GraphFile(final Graph<Integer> graph, final List<Tuple<String[],String>> sfDependencies, int mainArity) {
        this.graph = graph;
        this.sfDependencies = sfDependencies;
        this.mainArity = mainArity;
    }
}
