;;;
;;; Copyright (c) Sebastian Ertel 2013-2014. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.if-statement-test
  (:require [clojure.test :refer :all :as test]
            [com.ohua.lang :refer :all]
            [com.ohua.testing :refer :all :as ohua-test]
            [com.ohua.logging :as l]))

; Try to see if we can salvage anything here after #8 and #6

(ohua :import [com.ohua.lang.tests])

(deftest condition-direct-input
  "testing the case where all input comes actually from another function or is an argument."
  []
  (let [ohua-code (ohua
                    (if (< 5 (write "some-arg"))
                      (read (accept "some-id"))) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; 1 init stmt + 4 ops + 3 dependencies + 2 args + 1 condition + 1 compile stmt
         12)
      )
    (test/is (ohua-test/contains
               ohua-code
               '((new com.ohua.lang.compile.FlowGraphCompiler)
                  (.createOperator "com.ohua.lang/ifThenElse" 101)
                  (.createOperator "com.ohua.lang.tests/write" 103)
                  (.createOperator "com.ohua.lang.tests/read" 104)
                  (.createOperator "com.ohua.lang.tests/accept" 105)
                  (.registerDependency 101 0 105 -1)
                  (.registerDependency 103 -1 101 1)
                  (.registerDependency 105 -1 104 0)
                  (.setArguments
                    101
                    (clojure.core/into-array
                      com.ohua.lang.Tuple
                      (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'com.ohua.lang.Condition))))
                  (.setArguments
                    103
                    (clojure.core/into-array com.ohua.lang.Tuple
                                             (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
                  (.setArguments
                    105
                    (clojure.core/into-array com.ohua.lang.Tuple
                                             (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
                  (.compile true)))
             )
    ))

(deftest condition-indirect-input
  "testing the case where input comes from a variable."
  []
  (let [ohua-code (ohua
                    (let [[one _] (accept "some-id")]
                      (if (< 5 one)
                        (parse (read "some-id")))) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 4 ops + 3 dependencies + 2 args + 1 condition + compile stmt
         12))
    (test/is
      (ohua-test/contains
        ohua-code
        '(
           (.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/parse" 104)
           (.createOperator "com.ohua.lang.tests/read" 105)
           (.registerDependency 100 0 102 1)
           (.registerDependency 105 -1 104 0)
           (.registerDependency 102 0 105 -1))))
    ))

(deftest branch-direct-input
  "testing the case where all input comes actually from another function or is an argument."
  []
  (let [ohua-code (ohua
                    (if (< 5 10)
                      (read (accept "some-id"))) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 3 ops + 2 dependencies + 1 arg + 1 condition + compile stmt
         9))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang/ifThenElse" 101)
           (.createOperator "com.ohua.lang.tests/read" 103)
           (.createOperator "com.ohua.lang.tests/accept" 104)
           (.registerDependency 101 0 104 -1))))
    ))

(deftest branch-indirect-input
  "testing the case where input comes from a variable."
  []
  (let [ohua-code (ohua
                    (let [[one _] (accept "some-id")]
                      (if (< 5 10)
                        (read one))) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 3 ops + 2 dependencies + 1 arg + 1 condition + compile stmt
         9))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/read" 104)
           (.registerDependency 100 0 104 0)
           (.registerDependency 102 0 104 -1))))
    ))

(deftest branch-out-indexing
  "check whether inputs to the branches receive the proper out-idx"
  []
  (let [ohua-code (ohua
                    (let [[one two three four] (accept "some-id")]
                      (if (< 5 10)
                        (read one two)
                        (write three four))) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 4 ops + 6 dependencies + 1 arg + 1 condition + compile stmt
         14))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/read" 104)
           (.createOperator "com.ohua.lang.tests/write" 105)
           (.registerDependency 100 0 104 0)
           (.registerDependency 100 1 104 1)
           (.registerDependency 100 2 105 0)
           (.registerDependency 100 3 105 1)
           (.registerDependency 102 0 104 -1)
           (.registerDependency 102 1 105 -1))))
    ))

(deftest direct-condition-input
  "The condition is an operator."
  []
  (let [ohua-code (ohua
                    (if (accept "8080")
                      (read "arg0")
                      (write "arg1")) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 4 ops + 3 dependencies + 3 args + 1 condition + compile stmt
         13))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang/ifThenElse" 101)
           (.createOperator "com.ohua.lang.tests/accept" 103)
           (.createOperator "com.ohua.lang.tests/read" 104)
           (.createOperator "com.ohua.lang.tests/write" 105)
           (.registerDependency 103 -1 101 1)
           (.registerDependency 101 0 104 -1)
           (.registerDependency 101 1 105 -1)
           )))
    ))

(deftest all-out-indexing
  "all outputs go to different places (condition, if-branch, else-branch)"
  []
  (let [ohua-code (ohua
                    (let [[one two three] (accept "some-id")]
                      (if (< 10 one)
                        (read two)
                        (write one three))) :test-compile)]
    ;      (l/enable-logging )
    (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is
      (= (count ohua-code)
         ; init stmt + 4 ops + 6 dependencies + 1 arg + 1 condition + compile stmt
         14))
    (test/is
      (ohua-test/contains
        ohua-code
        '(
           (.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/read" 104)
           (.createOperator "com.ohua.lang.tests/write" 105)
           (.registerDependency 100 0 102 1)
           (.registerDependency 100 1 104 0)
           (.registerDependency 100 0 105 0)
           (.registerDependency 100 2 105 1)
           (.registerDependency 102 0 104 -1)
           (.registerDependency 102 1 105 -1))))
    ))

(deftest multiple-sources
  "input comes from different sources and is used by different parts of the if statement"
  []
  (let [ohua-code (ohua
                    (let [one (accept "some-id")
                          two (accept "some-other-id")
                          three (accept "some-additional-other-id")]
                      (if (< 10 one)
                        (read two)
                        (write three))) :test-compile)]
    ;   (l/enable-logging )
    (l/write ohua-code :dispatch clojure.pprint/code-dispatch)

    (test/is
      (= (count ohua-code)
         ; init stmt + 6 ops + 5 dependencies (3 from the accepts and 2 from the cond) + 3 arg + 1 condition + compile stmt
         17))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang.tests/accept" 101)
           (.createOperator "com.ohua.lang.tests/accept" 102)
           (.createOperator "com.ohua.lang/ifThenElse" 104)
           (.createOperator "com.ohua.lang.tests/read" 106)
           (.createOperator "com.ohua.lang.tests/write" 107)
           (.registerDependency 100 -1 104 1)
           (.registerDependency 101 -1 106 0)
           (.registerDependency 102 -1 107 0)
           (.registerDependency 104 0 106 -1)
           (.registerDependency 104 1 107 -1))))
    ))

(deftest if-two-args-test
  "The condition receives two arguments."
  []
  (let [ohua-code (ohua (let [prod1 (produce)
                              prod2 (produce)]
                          (if (= prod1 prod2) (consume prod1) (consume prod2))) :test-compile)]
    ;      (l/enable-logging )
    ;      (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    ; init stmt + 5 ops + 6 dependencies (4 from the produces and 2 from the cond) + 0 args + 1 condition + compile stmt
    (test/is (= (count ohua-code) 14))
    ; make sure the compound args have concrete locations
    (ohua-test/contains
      ohua-code '((.registerDependency 100 -1 103 1)
                   (.registerDependency 101 -1 103 2)))
    ))

(deftest if-one-arg-run-test
  ""
  []
  (let [input (map int (range 10))
        result (<-ohua
                 (smap
                   (fn [prod]
                     (if (< prod 3) (mark prod "if-result") (mark prod "else-result")))
                   input))]
    (test/is (= (reduce + (map first (filter #(= (second %) "if-result") result))) (+ 0 1 2)))
    (test/is (= (reduce + (map first (filter #(= (second %) "else-result") result))) (+ 3 4 5 6 7 8 9)))))

(deftest if-two-args-run-test
  ""
  []
  (let [input (map int (range 10))
        result (<-ohua
                 (smap
                   (fn [p]
                     (let [prod1 (peek p)
                           prod2 (peek p)]
                       (if (= prod1 prod2) (mark prod1 "if-result") (mark prod2 "else-result"))))
                   input))]
    (test/is (= (reduce + (map first (filter #(= (second %) "if-result") result))) (reduce + (range 10))))
    (test/is (= (reduce + (map first (filter #(= (second %) "else-result") result))) 0))))

(deftest select-with-compiled-condition
  []
  (l/enable-compilation-logging)
  (let [ohua-code (ohua
                    (consume
                      (if
                        (=
                          (accept "8080")
                          'test)
                        (read "arg0")
                        (write "arg1"))) :test-compile)]
    (l/enable-logging)
    (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is (= (count ohua-code) 19))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/consume" 100)
           (.createOperator "com.ohua.lang/select" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/accept" 104)
           (.createOperator "com.ohua.lang.tests/read" 105)
           (.createOperator "com.ohua.lang.tests/write" 106)
           (.registerDependency 101 -1 100 0)
           (.registerDependency 102 0 101 0)
           (.registerDependency 105 -1 101 1)
           (.registerDependency 106 -1 101 2)
           (.registerDependency 104 -1 102 1)
           (.registerDependency 102 0 105 -1)
           (.registerDependency 102 1 106 -1)))
      )))

(deftest select-with-direct-condition-input
  []
  (let [ohua-code (ohua
                    (consume
                      (if (accept "8080")
                        (read "arg0")
                        (write "arg1"))) :test-compile)]
    (test/is (= (count ohua-code) 19))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/consume" 100)
           (.createOperator "com.ohua.lang/select" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/accept" 104)
           (.createOperator "com.ohua.lang.tests/read" 105)
           (.createOperator "com.ohua.lang.tests/write" 106)
           (.registerDependency 101 -1 100 0)
           (.registerDependency 102 0 101 0)
           (.registerDependency 105 -1 101 1)
           (.registerDependency 106 -1 101 2)
           (.registerDependency 104 -1 102 1)               ; interestingly this is different as when input comes straight from an op. -> tested below: the switch does not care.
           (.registerDependency 102 0 105 -1)
           (.registerDependency 102 1 106 -1)))
      )))

(deftest two-branch-select-destructing
  []
  (let [ohua-code (ohua
                    (let [result (if (accept "8080")
                                   (read "arg0")
                                   (write "arg1"))]
                      (consume result)) :test-compile)]
    (test/is
      (= (count ohua-code)
         ; init stmt + 4 ops + 3 dependencies + 3 args + 1 condition + compile stmt
         19))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang/select" 100)
           (.createOperator "com.ohua.lang/ifThenElse" 101)
           (.createOperator "com.ohua.lang.tests/accept" 103)
           (.createOperator "com.ohua.lang.tests/read" 104)
           (.createOperator "com.ohua.lang.tests/write" 105)
           (.createOperator "com.ohua.lang.tests/consume" 106)
           (.registerDependency 100 -1 106 0)
           (.registerDependency 101 0 100 0)
           (.registerDependency 104 -1 100 1)
           (.registerDependency 105 -1 100 2)
           (.registerDependency 103 -1 101 1)
           (.registerDependency 101 0 104 -1)
           (.registerDependency 101 1 105 -1)))
      )))

(deftest single-branch-select
  "Testing the select creation for an if-statement with a single branches.
   Note: Of course the select in this case is overhead but we can optimize for this later."
  []
  (let [ohua-code (ohua
                    (consume
                      (if (accept "8080")
                        (read "arg0"))) :test-compile)]
    (test/is (= (count ohua-code) 15))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/consume" 100)
           (.createOperator "com.ohua.lang/select" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 102)
           (.createOperator "com.ohua.lang.tests/accept" 104)
           (.createOperator "com.ohua.lang.tests/read" 105)
           (.registerDependency 101 -1 100 0)
           (.registerDependency 102 0 101 0)
           (.registerDependency 105 -1 101 1)
           (.registerDependency 104 -1 102 1)
           (.registerDependency 102 0 105 -1)))
      )))


(deftest cond-stat-single
  "Tests the cond-statement with a single conditon-expression-pair."
  []
  (let [ohua-code (ohua
                    (let [one (accept "some-id")
                          two (accept "some-other-id")]
                      (cond (< 10 one) (read two)
                            ))
                    :test-compile)]
    ;(l/enable-logging)
    (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is
      (ohua-test/contains
        ohua-code
        '((new com.ohua.lang.compile.FlowGraphCompiler)
           (.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang.tests/accept" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 103)
           (.createOperator "com.ohua.lang.tests/read" 105)
           (.registerDependency 100 -1 103 1)
           (.registerDependency 101 -1 105 0)
           (.registerDependency 103 0 105 -1)
           (.setArguments
             100
             (clojure.core/into-array
               com.ohua.lang.Tuple
               (clojure.core/list
                 (com.ohua.lang.Tuple.
                   (clojure.core/int 0)
                   'java.lang.String))))
           (.setArguments
             101
             (clojure.core/into-array
               com.ohua.lang.Tuple
               (clojure.core/list
                 (com.ohua.lang.Tuple.
                   (clojure.core/int 0)
                   'java.lang.String))))
           (.setArguments
             103
             (clojure.core/into-array
               com.ohua.lang.Tuple
               (clojure.core/list
                 (com.ohua.lang.Tuple.
                   (clojure.core/int 0)
                   'com.ohua.lang.Condition))))
           (.compile true)))
      )
    ))

(deftest nested-if
  "Tests simple nested if construction."
  []
  (let [ohua-code (ohua
                    (let [one (accept "some-id")
                          two (accept "some-other-id")
                          three (accept "third-id")]
                      (if (< 10 one) (read two)
                                     (if (< 20 one) (write three))
                                     ))
                    :test-compile)]
    ;   (l/enable-logging)
    ;   (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is
      (= (count ohua-code) 26))
    (test/is
      (ohua-test/contains
        ohua-code
        '((.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang.tests/accept" 101)
           (.createOperator "com.ohua.lang.tests/accept" 102)
           (.createOperator "com.ohua.lang/ifThenElse" 104)
           (.createOperator "com.ohua.lang.tests/read" 106)
           (.createOperator "com.ohua.lang/scope" 107)
           (.createOperator "com.ohua.lang/ifThenElse" 109)
           (.createOperator "com.ohua.lang/scope" 111)
           (.createOperator "com.ohua.lang.tests/write" 112)
           (.registerDependency 100 -1 104 1)
           (.registerDependency 101 -1 106 0)
           (.registerDependency 102 -1 107 0)
           (.registerDependency 100 -1 109 1)
           (.registerDependency 107 0 111 0)
           (.registerDependency 111 0 112 0)
           (.registerDependency 104 0 106 -1)
           (.registerDependency 104 1 107 -1)
           (.registerDependency 104 1 109 -1)
           (.registerDependency 109 0 111 -1)))
      )))

(deftest if-with-let-on-branch
  "Has a let on the branch which is evaluated before the body of the let."
  []
  (let [ohua-code (ohua
                    (let [one (accept "some-id")
                          two (accept "some-other-id")]
                      (if (< 10 one)
                        (let [r (add (read two))] (write r))))
                    :test-compile)]
    (test/is
      (ohua-test/contains
        ohua-code
        '((new com.ohua.lang.compile.FlowGraphCompiler)
           (.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang.tests/accept" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 103)
           (.createOperator "com.ohua.lang.tests/add" 105)
           (.createOperator "com.ohua.lang.tests/read" 106)
           (.createOperator "com.ohua.lang.tests/write" 107)
           (.registerDependency 100 -1 103 1)
           (.registerDependency 101 -1 106 0)
           (.registerDependency 105 -1 107 0)
           (.registerDependency 106 -1 105 0)
           (.registerDependency 103 0 106 -1)
           (.setArguments
             100
             (clojure.core/into-array com.ohua.lang.Tuple
                                      (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.setArguments
             101
             (clojure.core/into-array com.ohua.lang.Tuple
                                      (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.setArguments
             103
             (clojure.core/into-array
               com.ohua.lang.Tuple
               (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'com.ohua.lang.Condition))))
           (.compile true))
        ))
    ))

(deftest if-with-let-and-destructing-on-branch
  []
  (let [ohua-code (ohua
                    (let [one (accept "some-id")
                          two (accept "some-other-id")]
                      (if (< 10 one)
                        (let [[r] (add (read two))] (write r))))
                    :test-compile)]
    ;   (l/enable-logging)
    ;   (l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is
      (ohua-test/contains
        ohua-code
        '((new com.ohua.lang.compile.FlowGraphCompiler)
           (.createOperator "com.ohua.lang.tests/accept" 100)
           (.createOperator "com.ohua.lang.tests/accept" 101)
           (.createOperator "com.ohua.lang/ifThenElse" 103)
           (.createOperator "com.ohua.lang.tests/add" 105)
           (.createOperator "com.ohua.lang.tests/read" 106)
           (.createOperator "com.ohua.lang.tests/write" 107)
           (.registerDependency 100 -1 103 1)
           (.registerDependency 101 -1 106 0)
           (.registerDependency 106 -1 105 0)
           (.registerDependency 105 0 107 0)
           (.registerDependency 103 0 106 -1)
           (.setArguments
             100
             (clojure.core/into-array com.ohua.lang.Tuple
                                      (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.setArguments
             101
             (clojure.core/into-array com.ohua.lang.Tuple
                                      (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.String))))
           (.setArguments
             103
             (clojure.core/into-array
               com.ohua.lang.Tuple
               (clojure.core/list (com.ohua.lang.Tuple. (clojure.core/int 0) 'com.ohua.lang.Condition))))
           (.compile true))
        ))
    ))

(deftest local-as-condition-true
  "Solely a reference to a local is passed into the condition slot."
  []
  (test/is
    (=
      200
      (com.ohua.lang/<-ohua
        (let [one (id (int 100))]
          (if one
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
      (com.ohua.lang/<-ohua
        (let [one (id (int 100))
              c (id false)]
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
      (com.ohua.lang/<-ohua
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
      (com.ohua.lang/<-ohua
        (if (id false) 100 0))
      )
    )
  )

(deftest independent-function-on-condition
  "Independent function on the condition."
  []
  (test/is
    (=
      0
      (com.ohua.lang/<-ohua
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
      (com.ohua.lang/<-ohua
        (let [one (id (int 100))]
          (if false
            (add one 100)
            (subtract one 100))))
      )
    )
  )

;(deftest let-on-condition
;  "Usage of let inside condition."
;  []
;  (test/is
;    (=
;      0
;      (com.ohua.lang/<-ohua
;        (let [one (id (int 100))]
;          (if (<
;                (let [two (add one 200)]
;                   two)
;                 100)
;            (add one 100)
;            (subtract one 100))))
;      )
;    )
;  )


; TODO like above
;(deftest cond-stat-double
;  "Tests the cond-statement with two conditon-expression-pairs."
;  []
;  (let [ohua-code (ohua
;                   (let [one (accept "some-id")
;                         two (accept "some-other-id")
;                         three (accept "third-id")]
;                   (cond
;                     (< 10 one) (read two)
;                     (< 20 one) (read three)))
;                   :test-compile)]
;   (test/is
;     (= (count ohua-code)
;        ; init stmt + 6 ops + 7 dependencies (4 from the accepts and 3 from the cond) + 3 arg + 2 condition + compile stmt
;     20))))
;

(deftest if-select-run-test
  "Tests the select operator, which is needed if the if's outcome is used."
  []
  (let [input (map int (range 10))
        result (<-ohua (smap (fn [prod]
                               (if (< prod 3) (add prod 100) (subtract prod 3)))
                             input))]
    (test/is
      (= (reduce + result) 324)))
  )

(deftest check-direct-cond-input
  []
  "Input to the switch comes from another operator that evaluated the expression."
  (let [input (map int (range 10))
        result (<-ohua (smap (fn [prod]
                               (if (greaterThan prod 3) (add prod 100) (subtract prod 3)))
                             input))]
    (test/is
      (= (reduce + result) 324)))
  )


;(deftest static-cond-test-run
;  ""
;  []
;  (let [ohua-code (ohua (if (< 3 10) (accept) (read)))]
;    (test/is
;      ohua-code
;      1)))



; ----------

;(deftest indirect-output
; "testing the case where the output of an if is actually inside a let."
; []
; (test/is
;   (= (count (ohua
;               (let [[one _] (if (< 5 10)
;                               (accept one)) ]
;                 (read one)) :test-compile))
;      ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
;   8))
; )

;(deftest direct-output
; "testing the case where the output of an if is directly connected to the next function."
; []
; (test/is
;   (= (count (ohua
;               (read (if (< 5 10)
;                       (accept one))) :test-compile))
;      ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
;   8))
; )
;
;(deftest unbound-output
; "testing the case where the output of an if is not connected anymore.
;  (the branches are final.)"
; []
; (test/is
;   (= (count (ohua
;               (let [[one _] (accept)]
;                 (if (< 5 10) (write (read one)))
;                 (reply (read one))) :test-compile))
;      ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
;   8))
; )

;(deftest simple-if-else
; "testing the basic if clause"
; []
; (test/is
;   (= (count (ohua
;               (let [[one two _] (accept "input")]
;                 (if (< one 10)
;                   (read one)
;                   (write two))) :test-compile))
;      ; init stmt + 4 ops + 3 dependencies + 1 arg + compile stmt
;   10))
; )
;
;(deftest single-if
; "testing the case where only an if branch was provided"
; []
; (test/is
;   (= (count (ohua
;               (let [[one two _] (accept "input")]
;                 (if (< one 10)
;                   (read one))) :test-compile))
;      ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
;   8))
; )
