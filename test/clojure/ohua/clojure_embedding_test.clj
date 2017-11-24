(ns ohua.clojure-embedding-test
  (:require [ohua.lang :refer [<-ohua ohua ohua-require defsfn]]
            [clojure.test :refer [deftest is]]))

(deftest simple-embedding-test
  (is
    (=
      14
      (<-ohua
        (- (+ 5 (int 10)) 1)))))


(defsfn set-a (atom nil) [a new-val]
  (reset! a new-val)
  a)

(deftest stateful-clojure-functions-test 
  (is
    (<-ohua 
      (let [val (set-a 10)]
        (and
          (instance? clojure.lang.Atom val)
          (= 10 (deref val))))
      {:logging :debug})))
