(ns ohua.algo-test
  (:require [ohua.lang :refer [ohua <-ohua algo defalgo ohua-require]]
            [clojure.test :refer [deftest is]]))

(ohua-require [ohua.tests :refer [addLong multLong negateLong]])

(defalgo square [a]
  (multLong a a))


; this causes a runtime bug.
; java.lang.IndexOutOfBoundsException: Index: 0, Size: 0
; in ohua.tests/multLong-1
(deftest simple-algo-test 
  (is (= -144
        (<-ohua
          (negateLong (square 12))))))

(deftest simple-algo-test-2
  (is (= -144
        (<-ohua
          (negateLong (square (id 12)))))))
