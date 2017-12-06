;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.local-scoping-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer [ohua <-ohua algo]]
            [clojure.pprint :refer :all]
            [clojure.string :refer [join]]
            [ohua.testing :refer :all :as ohua-test]
            [clojure.walk :as walk]))

(ohua :import [ohua.tests])

; try this once #10 is done

(deftest local-scopes-in-clojure
  "For now this is a simple test for scopes of locals."
  []
  ; the inner-most closure redefines 'a'.
  (let [a "something"]
    (let [a (join " " [a "else"])]
      (test/is a "something else"))))

(deftest runtime-integration
  "Runs a small program and makes sure that the scope function is executed properly."
  ;  (set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) "../test-output/runtime-integration-flow")
  (let [input (map int (range 10))
        result (<-ohua
                 (smap
                   (fn [prod]
                     (if (< prod 3)
                       (add prod 100)
                       (if (< prod 5) (add prod 200) (subtract prod 3))))
                   input) {:logging :debug})]
    (test/is
      (= (reduce + result) 730)))
  )

; (deftest scope-function-compoundness
;   "Runs a small program and where the scope function has more than one argument. 
;    This test makes sure that the code for handling compound argument lists is working properly."
;   ;  (l/enable-compilation-logging )
;   ;  (set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) "../test-output/runtime-integration-flow")
;   (let [input (map int (range 10))
;         result (<-ohua
;                  (smap
;                    (fn [p]
;                      (let [prod (peek p) prod2 (peek p)]
;                        (if (< prod 3)
;                          (add prod 100)
;                          (if (< prod2 5) (add prod2 200) (subtract prod 3)))
;                        ))
;                    input)
;                   {:logging :debug})]
;     (test/is
;       (= (reduce + result) 730)))
;   )


; (deftest local-name-overshadowing
;   "Here, 'key' is defined as a local variable. However, our algorithm for scoping local variables does not identify it as such."
;   (let [data (into () [(into-array Object (list 30 (list (int 100))))
;                        (into-array Object (list 40 (list (int 200))))])
;         result (<-ohua
;                  (smap
;                    (algo [ [key b] ]
;                          (if (< 5 10)
;                            (id key)
;                            (smap (algo [c] (add key c)) b)))
;                    data))]
;     (test/is 30 (first result))
;     (test/is 40 (second result))))
