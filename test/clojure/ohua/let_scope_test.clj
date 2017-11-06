(ns ohua.let-scope-test
  (:require [ohua.lang :refer [<-ohua ohua-require algo]]
            [clojure.test :refer [deftest is]]))

(ohua-require [ohua.tests :refer :all])


(deftest simple-let-scope-test
  (is
    (=
      4
      (let [x (int 2)]
        (<-ohua
          (id (add x x)))))))


(def my-algo
  (let [x (int 5)]
    (algo [y] (add y x))))


(deftest scope-test-with-algo
  (is
    7
    (let [o (int 2)]
      (<-ohua (my-algo o)))))
