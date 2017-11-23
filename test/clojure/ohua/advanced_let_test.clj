;;;
;;; Copyright (c) Sebastian Ertel 2014. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.advanced-let-test
  (:require [clojure.test :refer :all :as test]
            [ohua.testing :refer :all :as ohua-test]
            [ohua.lang :refer [ohua]]
            [ohua.testutils :refer [compat-compile]]))

(ohua :import [ohua.tests])

(deftest embedded-let-compile-test
  "Tests the proper dataflow tracking in the let statement -> last statement provides result."
  []
;  (l/enable-compilation-logging )
  (let [ohua-code  (compat-compile
                     (let [request (accept "8080")]
                       (send (let [[one two] (read request)]
                               (parse one)
                               (parse two)))) :strip-ns)]
;    (l/enable-logging )
;    (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is
      (ohua-test/compare-code
        ohua-code
        '((new ohua.lang.compile.FlowGraphCompiler)
           (.createOperator "accept" 100)
           (.createOperator "send" 101)
           (.createOperator "read" 102)
           (.createOperator "parse" 103)
           (.createOperator "parse" 104)
           (.registerDependency 104 -1 101 0)
           (.registerDependency 100 -1 102 0)
           (.registerDependency 102 0 103 0)
           (.registerDependency 102 1 104 0)
           (.setArguments
				         100
				         (clojure.core/into-array ohua.lang.Tuple
                                          (clojure.core/list (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.compile true)))))
  )

(defn mytrace [a & msgs] (apply println a msgs) a)


; See Issue #17
; (deftest embedded-let-run-test
;   "Conceptually the same as above but this time we execute it."
;   []
;   (def result-cond (long-array 10))
;   (ohua (let [prod (produceFn)]
;           (ohua.tests/collect (let [[one two] (balance prod 2 2)]
;                      (add (mytrace one "one") 100)
;                      (add (mytrace two "two" one) 3))
;                    result-cond)))
;     (test/is
;         (= (reduce + result-cond) (+ (reduce + (range 1 10 2)) 15)))
;   )

(deftest nested-let-compile-test
  "Tests the propagation of the let information."
  []
;  (l/enable-compilation-logging )
  (let [ohua-code  (compat-compile
                     (let [request (accept "8080")]
                       (send
                         (let [one (read request)]
                           (let [two (parse one)]
                             (parse two))))) :strip-ns)]
;    (l/enable-logging )
    (test/is
      (ohua-test/compare-code
        ohua-code
        '((new ohua.lang.compile.FlowGraphCompiler)
           (.createOperator "accept" 100)
           (.createOperator "send" 101)
           (.createOperator "read" 102)
           (.createOperator "parse" 103)
           (.createOperator "parse" 104)
           (.registerDependency 104 -1 101 0)
           (.registerDependency 100 -1 102 0)
           (.registerDependency 102 -1 103 0)
           (.registerDependency 103 -1 104 0)
           (.setArguments
				         100
				         (clojure.core/into-array ohua.lang.Tuple
                                          (clojure.core/list (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.compile true)))))
  )
