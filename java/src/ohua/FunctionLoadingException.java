package ohua;

import java.util.List;
import java.util.stream.Collectors;

import ohua.util.Tuple;
import ohua.util.Conversions;

final class FunctionLoadingException extends Exception {
    public final List<Tuple<String[], String>> missing;

    public FunctionLoadingException(final List<Tuple<String[], String>> missing) {
        this.missing = missing;
    }

    @Override
    public String toString() {
        return "FunctionLoadingException: missing the following stateful functions: " + missing.stream().map(Conversions::sfRefToString).collect(Collectors.joining(", "));
    }
}
