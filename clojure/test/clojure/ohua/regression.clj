(ns ohua.regression
  (:require [clojure.test :as test]))


(defn -main []
  (let [namespaces '[ohua.advanced-let-test
                     ohua.algo-test
                     ohua.apply-test
                    ;  ohua.clojure-embedding-test  ; excluded until we deal with issue #5
                     ohua.compile-test
                    ;  ohua.destructuring-test  ; excluded until we deal with issue #18
                     ohua.enum-class-literal-test
                     ohua.execution-test
                     ohua.explicit-schema-match-test
                     ohua.higher-order-functions-test
                     ohua.if-context-function-test
                     ohua.if-statement-test
                     ohua.let-scope-test
                     ohua.linker-test
                    ;  ohua.linker-test-2  this one must be ran manually, as the way leinigen executes tests makes it infeasible otherwise
                     ; ohua.local-scoping-test ; disabled because it uses smap which is broken
                     ohua.local-variable-override-test
                     ohua.namespace-test
                     ohua.null-values-test
                     ohua.simple-run-test
                     ohua.smap-test]]
  (apply require namespaces)
  (apply test/run-tests namespaces)))
