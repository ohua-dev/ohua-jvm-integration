package ohua.util;

import ohua.util.Tuple;

public abstract class Conversions {
    private Conversions() {}

    public static String sfRefToString(Tuple<String[],String> ref) {
        return Conversions.sfRefToString(ref._s, ref._t);
    }

    public static String sfRefToString(String[] nsRef, String fName) {
        return Conversions.nsRefToString(nsRef) + "/" + fName;
    }

    public static String nsRefToString(String[] ref) {
        return String.join(".", ref);
    }
}
