(ns ohua.simple-run-test
  (:require [ohua.lang :refer [ohua <-ohua ohua-require]]
            [clojure.test :refer [deftest is]]))

(ohua-require [ohua.tests :refer [trace]])

(deftest run-id
  (is (= 9
        (<-ohua (id 9))))
  (is (= 9
        (<-ohua (trace (trace 9)))))
    )
