;;;
;;; Copyright (c) Sebastian Ertel 2013-2014. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.if-statement-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.testing :refer :all :as ohua-test]
            [ohua.testutils :refer [compat-compile]]))

; Something in smap is broken right now. Add the smap test cases in here back once we have that fixed

(ohua :import [ohua.tests])

(deftest condition-direct-input
  "testing the case where all input comes actually from another function or is an argument."
  []
  (let [ohua-code (compat-compile
                    (if (< 5 (write "some-arg"))
                      (read (accept "some-id"))
                      nil) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; 1 init stmt + 7 ops + 8 dependencies + 5 args + 1 compile stmt
         21)
      )
    (test/is (ohua-test/contains
               ohua-code
               '((new ohua.lang.compile.FlowGraphCompiler)
                  (.createOperator "ohua.lang/bool" 107)
                  (.createOperator "ohua.tests/write" 101)
                  (.createOperator "ohua.tests/read" 102)
                  (.createOperator "ohua.tests/accept" 103)
                  (.registerDependency 107 0 103 -1)
                  (.registerDependency 101 -1 105 2)
                  (.registerDependency 105 -1 107 0)
                  (.registerDependency 103 -1 102 0)
                  (.setArguments
                    101
                    (clojure.core/into-array ohua.lang.Tuple
                                             (clojure.core/list (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
                  (.setArguments
                    103
                    (clojure.core/into-array ohua.lang.Tuple
                                             (clojure.core/list (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
                  (.compile true)))
             )
    ))

(deftest local-as-condition-true
  "Solely a reference to a local is passed into the condition slot."
  []
  (test/is
    (=
      200
      (ohua.lang/<-ohua
        (let [one (id (int 100))]
          (if (= 100 one)
            (add one 100)
            (subtract one 100))))
      )
    )
  )

(deftest local-as-condition-false
  "Solely a reference to a local is passed into the condition slot."
  []
  (test/is
    (=
      0
      (ohua.lang/<-ohua
        (let [one (id (int 100))
              c false]
          (if c
            (add one 100)
            (subtract one 100))))
      )
    )
  )

(deftest values-only
  "Values only passed to the branches."
  []
  (test/is
    (=
      0
      (ohua.lang/<-ohua
        (let [one (id (int 100))
              c (id false)
              added (add one 100)
              subtracted (subtract one 100)]
          (if c
            added
            subtracted)))
      )
    )
  )


(deftest independent-functions-on-branches
  "Independent functions on the branches."
  []
  (test/is
    (=
      0
      (ohua.lang/<-ohua
        (if false 100 0))
      )
    )
  )

(deftest independent-function-on-condition
  "Independent function on the condition."
  []
  (test/is
    (=
      0
      (ohua.lang/<-ohua
        (let [one (id (int 100))]
          (if (id false)
            (add one 100)
            (subtract one 100))))
      )
    )
  )


(deftest static-on-condition
  "Independent function on the condition."
  []
  (test/is
    (=
      0
      (ohua.lang/<-ohua
        (let [one (id (int 100))]
          (if false
            (add one 100)
            (subtract one 100))))
      )
    )
  )

(deftest let-on-condition
 "Usage of let inside condition."
 []
 (test/is
   (=
     0
     (ohua.lang/<-ohua
       (let [one (id (int 100))]
         (if (<
               (let [two (add one 200)]
                  two)
                100)
           (add one 100)
           (subtract one 100))))
     )
   )
 )


; (deftest if-select-run-test
;   "Tests the select operator, which is needed if the if's outcome is used."
;   []
;   (let [input (map int (range 10))
;         result (<-ohua (smap (fn [prod]
;                                (if (< prod 3) (add prod 100) (subtract prod 3)))
;                              input))]
;     (test/is
;       (= (reduce + result) 324)))
;   )

; (deftest check-direct-cond-input
;   []
;   "Input to the switch comes from another operator that evaluated the expression."
;   (let [input (map int (range 10))
;         result (<-ohua (smap (fn [prod]
;                                (if (greaterThan prod 3) (add prod 100) (subtract prod 3)))
;                              input))]
;     (test/is
;       (= (reduce + result) 324)))
;   )
