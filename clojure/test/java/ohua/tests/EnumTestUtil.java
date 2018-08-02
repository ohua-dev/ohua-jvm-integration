package ohua.tests;

import ohua.lang.defsfn;

/**
 * Created by justusadam on 15/06/16.
 */
public class EnumTestUtil {

    public enum AnEnum {
        Value1, Value2
    }

    public static class EnumRecognizer1 {
        @defsfn
        public boolean isValue1(int pos, Object... vals) {
            AnEnum val = (AnEnum) vals[pos];

            switch (val) {
                case Value1:
                    return true;
                default:
                    return false;
            }
        }
    }

    public static class EnumRecognizer2 {
        @defsfn
        public boolean isValue2(int pos, Object... vals) {
            AnEnum val = (AnEnum) vals[pos];

            switch (val) {
                case Value2:
                    return true;
                default:
                    return false;
            }
        }
    }
}
