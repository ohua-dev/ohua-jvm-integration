(ns ohua.smap-test
  (:require [clojure.test :refer [deftest is]])
  (:use ohua.lang))

(ohua-require [ohua.tests :refer [add]])


(deftest smap-no-op-test
  (let [l [0 1 2 3 4]]
    (is 
      (= l
        (<-ohua (smap (algo [i] (id i)) l))))
    ; this should work as well actually
    ; (is 
    ;   (= l
    ;     (<-ohua (smap id l))))
    ))

(deftest simple-smap-test
  (let [l (range 1 1000)]
    (is 
      (= (map #(+ 4 %) l)
        (<-ohua (smap (algo [i] (+ i 4)) l))))))


(deftest smap-if-test
  (let [l (range 1 1000)]
    (is 
      (= (map #(if (= 0 (/ 3 %)) (inc %) (dec %)) l)
        (<-ohua (smap (algo [i] (if (= 0 (/ 3 i)) (inc i) (dec i))) l))))))


; doesn't work because destructuring doesn't work
; (deftest smap-if-test-w-destructure
;   (let [l (map vector (range 1000 1 -1) (range 1 1000))]
;     (is 
;       (= (map (fn [[a b]] (if (= 0 (/ 3 a)) (+ a b) (- a b))) l)
;         (<-ohua (smap (algo [[a b]] (if (= 0 (/ 3 a)) (+ a b) (- a b))) l))))))
