package ohua.tests;

import ohua.lang.defsfn;

/**
 * Created by justusadam on 09/03/16.
 */
public abstract class Constants {

    public static class Const5 {
        @defsfn
        public Integer const5() {
            return 5;
        }
    }

    public static class Const1 {
        @defsfn
        public Integer const1() {
            return 1;
        }
    }

    public static class Const {
        @defsfn
        public Long _const(Long c) {
            return c;
        }
    }

}
