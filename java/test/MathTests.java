import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

import static java.nio.file.Paths.get;

import ohua.Runner;

class MathTests {
    @Test
    void simpleRun() throws Exception {
        try {
            Runner.CallableAlgorithm algo = Runner.loadFile(get("test/math_tests.ohuao"));
            Integer a = 1;
            Integer b = 2;
            Integer c = 3;
            Integer d = 4;
            int result = (Integer) algo.call(a,b,c,d);
            assertEquals(result, b - c + d * d);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
}
