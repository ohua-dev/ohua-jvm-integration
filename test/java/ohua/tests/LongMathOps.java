package ohua.tests;

import ohua.lang.defsfn;

public abstract class LongMathOps {

    public static class Add {
        @defsfn
        public long addLong(long a, long b) {
            return a + b;
        }
    }

    public static class Mult {
        @defsfn
        public long multLong(long a, long b) {
            return a * b;
        }
    }

    public static class Negate {
        @defsfn
        public long negateLong(long l) {
            return 0 - l;
        }
    }

}
