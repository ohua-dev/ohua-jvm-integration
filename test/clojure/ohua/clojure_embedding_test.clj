(ns ohua.clojure-embedding-test
  (:require [ohua.lang :refer [<-ohua ohua ohua-require]]
            [clojure.test :refer [deftest is]]))

(deftest simple-embedding-test
  (is
    (=
      14
      (<-ohua
        (- (+ 5 (int 10)) 1)))))
