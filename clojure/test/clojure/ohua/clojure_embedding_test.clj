(ns ohua.clojure-embedding-test
  (:require [ohua.lang :refer [<-ohua ohua ohua-require defsfn]]
            [clojure.test :refer [deftest is]]))

(deftest simple-embedding-test
  (is
    (=
      14
      (<-ohua
        (- (+ 5 (int 10)) 1)))))

(defn atom? [a]
  (instance? clojure.lang.Atom a))


(defn ^{:init-state '(atom nil)} set-b [a new-val]
  (reset! a new-val)
  a)

(deftest explicit-stateful-clojure-functions-test 
  (is (not (nil? (get :init-state (meta set-b)))))
  (is (atom? (eval (get :init-state (meta set-b)))))
  (is
    (<-ohua 
      (let [val (set-b 10)]
        (and
          (atom? val)
          (= 10 (deref val))))
      )))

(defsfn set-a (atom nil) [a new-val]
  (reset! a new-val)
  a)

(deftest stateful-clojure-functions-test 
  (is (not (nil? (get :init-state (meta set-a)))))
  (is (atom? (eval (get :init-state (meta set-a)))))
  (is
    (<-ohua 
      (let [val (set-a 10)]
        (and
          (atom? val)
          (= 10 (deref val))))
      )))
