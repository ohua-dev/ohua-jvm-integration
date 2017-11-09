(ns ohua.regression
  (:require [clojure.test :as test]))


(defn -main []
  (let [namespaces '[ohua.advanced-let-test
                     ohua.algo-test
                     ohua.apply-test
                     ohua.compile-test
                     ohua.execution-test
                     ohua.explicit-schema-match-test
                     ohua.linker-test
                     ohua.linker-test-2
                     ohua.local-variable-override-test
                     ohua.null-values-test
                     ohua.simple-run-test]]
  (apply require namespaces)
  (apply test/run-tests namespaces)))
