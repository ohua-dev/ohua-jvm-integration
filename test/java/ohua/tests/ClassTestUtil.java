package ohua.tests;

import ohua.lang.defsfn;

/**
 * Created by justusadam on 15/06/16.
 */
public class ClassTestUtil {
    public static class MyClass {
    }

    public static class ClassRecognizer {

        @defsfn
        public boolean isClass(int pos, Object... vals) {
            return vals[pos] instanceof Class;
        }
    }
}
