import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

import static java.nio.file.Paths.get;

import ohua.Runner;

class MathTests {
    @Test
    void simpleRun() throws Exception {
        Runner.CallableAlgorithm a = Runner.loadFile(get("test/math_tests.ohuao"));
        a.call(1,2,3,4);
    }
}
