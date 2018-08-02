package ohua.graph;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonArray;
import javax.json.JsonString;

import java.util.List;

import java.nio.file.Path;

import ohua.util.Lazy;
import ohua.graph.*;
import ohua.util.Tuple;

import java.io.IOException;

import static java.nio.file.Files.newBufferedReader;

public abstract class JsonReader {

    private JsonReader () {}

    public static GraphFile read(Path graphFile) throws IOException {
        try (javax.json.JsonReader reader = Json.createReader(newBufferedReader(graphFile))) {
            return JsonReader.parseGraphFile(reader.readObject());
        }
    }

    private static GraphFile parseGraphFile(JsonObject o) {
        return new GraphFile(JsonReader.parseGraph(o.getJsonObject("graph")),
                             JsonReader.parseSfDeps(o.getJsonArray("sfDependencies")),
                             o.getInt("mainArity"));
    }

    private static Graph<Integer> parseGraph(JsonObject o) {
        return new Graph((Operator[]) o.getJsonArray("operators")
                         .getValuesAs(JsonReader::parseOperator).toArray(),
                         (Arc[]) o.getJsonArray("arcs").getValuesAs(JsonReader::parseArc).toArray());
    }

    private static Operator parseOperator (JsonObject o) {
        return new Operator(o.getInt("id"), JsonReader.sfRefToString(JsonReader.parseSfDep(o.getJsonObject("type"))));
    }

    private static List<Tuple<String[],String>> parseSfDeps(JsonArray arr) {
        return arr.getValuesAs(JsonReader::parseSfDep);
    }

    private static Tuple<String[],String> parseSfDep(JsonObject o) {
        return new Tuple<>((String[]) o.getJsonArray("namespace").getValuesAs(JsonString::getString).toArray(),
                           o.getString("name"));
    }

    private static String sfRefToString(Tuple<String[], String> sfRef) {
        return String.join(".", sfRef._s) + "/" + sfRef._t;
    }

    private static Arc<Integer> parseArc(JsonObject o) {
        return new Arc<>(JsonReader.parseTarget(o.getJsonObject("target")),
                         JsonReader.parseSource(o.getJsonObject("source")));
    }

    private static Target parseTarget(JsonObject o) {
        return new Target(o.getInt("operator"), o.getInt("index"));
    }

    private static Source<Integer> parseSource(JsonObject o) {
        String t = o.getString("type");
        switch (t) {
        case "local":
            return new Source.Local(JsonReader.parseTarget(o.getJsonObject("val")));
        case "env":
            return new Source.Env<>(o.getInt("val"));
        default:
            throw new RuntimeException("Invalid type for source: " + t);
        }
    }
}
