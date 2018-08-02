(ns ohua.algo-test
  (:require [ohua.lang :refer [ohua <-ohua algo defalgo ohua-require]]
            [clojure.test :refer [deftest is]]))

(ohua-require [ohua.tests :refer [addLong multLong negateLong]])

(defalgo square [a]
  (multLong a a))


(deftest simple-apply-test
  (is (= -144
        (<-ohua
          (negateLong (square 12))))))

(deftest simple-algo-test-2
  (is (= -144
        (<-ohua
          (negateLong (square (id 12)))))))
