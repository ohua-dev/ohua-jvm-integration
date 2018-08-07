package ohua;

import java.nio.file.Path;
import java.util.stream.IntStream;
import java.util.function.Supplier;
import java.util.Arrays;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import ohua.graph.JsonReader;
import ohua.util.*;
import ohua.graph.GraphFile;
import ohua.graph.Graph;
import ohua.graph.Arc;
import ohua.graph.Source;
import ohua.loader.JavaProviderFromAnnotatedMethod;

/**
 * Top level interface for the ohua-java integration.
 */
public abstract class Runner {
    private Runner() {}

    /**
     * An object encapsulating the runtime representation of an ohua graph.
     */
    public static final class CallableAlgorithm {
        private final Runnable runnableGraph;
        private final MutableBox<Object[]> initarr;

        CallableAlgorithm(final Runnable graph, final MutableBox<Object[]> initarr) {
            this.runnableGraph = graph;
            this.initarr = initarr;
        }
        /**
         * Run the algorithm with the supplied arguments. The behaviour when
         * calling this function from two threads simultaneously is unspecified.
         *
         * Note that currently there is no way to reset the state of the graph,
         * so unless all your operators are pure you may obtain unexpected
         * results from calling this function again.
         */
        public Object call(Object... args) {
            this.initarr.set(args);
            this.runnableGraph.run();
            return null;
        }
    }

    private static void ensureStatefulFunctionsAreLoaded(Iterable<Tuple<String[], String>> functions) throws FunctionLoadingException {
        StatefulFunctionProvider provider = new JavaProviderFromAnnotatedMethod();
        List missing = new ArrayList<>();
        for (Tuple<String[], String> ref : functions) {
            if (!provider.exists(Conversions.nsRefToString(ref._s), ref._t)) {
                missing.add(ref);
            }
        }
        if (!missing.isEmpty()) {
            throw new FunctionLoadingException(missing);
        }
    }

    public static CallableAlgorithm createCallable(final GraphFile gf) throws FunctionLoadingException {
        Runner.ensureStatefulFunctionsAreLoaded(gf.sfDependencies);
        int ma = gf.mainArity;
        final MutableBox<Object[]> mbox = MutableBox.empty();
        final Lazy<Object>[] initarr = IntStream.range(0, ma).boxed().map(i -> Lazy.createLazy(() -> mbox.get()[i])).toArray(Lazy[]::new);
        Graph<Integer> gr = gf.graph;
        Arc<Lazy<Object>>[] newArcs = Arrays.stream(gr.arcs).map(arc -> {
                if (arc.source instanceof Source.Env) {
                    Source.Env se = (Source.Env) arc.source;
                    int i = (Integer) se.hostExpr;
                    return new Arc(arc.target, new Source.Env(initarr[i]));
                } else {
                    return arc;
                }
            }).toArray(Arc[]::new);
        Runnable r = Runtime.prepare(new Graph (gr.operators, newArcs));
        return new CallableAlgorithm(r, mbox);
    }

    /**
     * Read the graph description from the provided file and compile it into a
     * runnable format.
     */
    public static CallableAlgorithm loadFile(final Path graphFile) throws IOException, FunctionLoadingException {
        return Runner.createCallable(JsonReader.read(graphFile));
    }
}
