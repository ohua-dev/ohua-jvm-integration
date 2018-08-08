package ohua;

import java.nio.file.Path;
import java.util.stream.IntStream;
import java.util.function.Supplier;
import java.util.Arrays;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.Comparator;
import java.util.stream.Stream;
import java.util.function.Function;

import ohua.graph.JsonReader;
import ohua.util.*;
import ohua.graph.GraphFile;
import ohua.graph.Graph;
import ohua.graph.Arc;
import ohua.graph.Operator;
import ohua.graph.Source;
import ohua.graph.Target;
import ohua.loader.JavaProviderFromAnnotatedMethod;
import ohua.lang.defsfn;

import ohua.runtime.engine.flowgraph.elements.operator.OperatorFactory;

/**
 * Top level interface for the ohua-java integration.
 */
public abstract class Runner {
    private static final String captureMethodRef;
    private Runner() {}
    static {
        Class<Runner.CaptureOperator> cls = Runner.CaptureOperator.class;
        captureMethodRef = cls.getName() + "/" + "capture";
        OperatorFactory.registerUserOperator(captureMethodRef, cls.getName());
    }

    /**
     * An object encapsulating the runtime representation of an ohua graph.
     */
    public static final class CallableAlgorithm {
        private final Runnable runnableGraph;
        private final MutableBox<Object[]> initarr;
        private final AtomicReference<Object> captureRef;

        CallableAlgorithm(final Runnable graph, final MutableBox<Object[]> initarr, AtomicReference<Object> captureRef) {
            this.runnableGraph = graph;
            this.initarr = initarr;
            this.captureRef = captureRef;
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
            return captureRef.get();
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

    public static class CaptureOperator {
        public CaptureOperator () {}
        @defsfn
        public void capture(final AtomicReference<Object> ref, final Object o) {
            ref.set(o);
        }
    }

    public static CallableAlgorithm createCallable(final GraphFile gf)
        throws FunctionLoadingException {
        Runner.ensureStatefulFunctionsAreLoaded(gf.sfDependencies);
        int ma = gf.mainArity;

        // Prepare the lazy references for the main argument. These have to be
        // filled before running the graph.
        final MutableBox<Object[]> mbox = MutableBox.empty();
        final Lazy<Object>[] initarr = IntStream.range(0, ma).boxed()
            .map(i -> Lazy.createLazy(() -> mbox.get()[i]))
            .toArray(Lazy[]::new);

        Graph<Integer> gr = gf.graph;

        // Generates an operator to capture the result.
        AtomicReference<Object> ref = new AtomicReference<>();
        int opIndex = Arrays.stream(gr.operators).map(op0 -> op0.id).max(Comparator.comparing(Function.identity())).orElse(0) + 1;
        Arc<Lazy<Object>> returnArc = new Arc(new Target(opIndex, 1), new Source.Local(gf.returnArc));
        Arc<Lazy<Object>> returnRefArc = new Arc(new Target(opIndex, 0), new Source.Env(Lazy.createRealized(ref)));

        Stream<Arc<Lazy<Object>>> remappedArcs = Arrays.stream(gr.arcs).map(arc -> {
                if (arc.source instanceof Source.Env) {
                    Source.Env se = (Source.Env) arc.source;
                    int i = (Integer) se.hostExpr;
                    return new Arc<>(arc.target, new Source.Env(initarr[i]));
                } else {
                    return arc;
                }
            });
        Arc<Lazy<Object>>[] newArcs =
            Stream.concat(remappedArcs, Stream.of(returnArc, returnRefArc))
                  .toArray(Arc[]::new);
        Operator[] newOperators =
            Stream.concat(Arrays.stream(gr.operators),
                          Stream.of(new Operator (opIndex, captureMethodRef)))
            .toArray(Operator[]::new);

        Runnable r = Runtime.prepare(new Graph (newOperators, newArcs));
        return new CallableAlgorithm(r, mbox, ref);
    }

    /**
     * Read the graph description from the provided file and compile it into a
     * runnable format.
     */
    public static CallableAlgorithm loadFile(final Path graphFile)
        throws IOException, FunctionLoadingException {
        return Runner.createCallable(JsonReader.read(graphFile));
    }
}
