package test_operators;

import ohua.lang.defsfn;

public abstract class MathOps {
    public static final class Add {
        @defsfn
        public Integer add(Integer a, Integer b) {
            return a + b;
        }
    }

    public static final class Sub {
        @defsfn
        public Integer sub(Integer a, Integer b) {
            return a - b;
        }
    }

    public static final class Mult {
        @defsfn
        public Integer mult(Integer a, Integer b) {
            return a * b;
        }
    }
}
