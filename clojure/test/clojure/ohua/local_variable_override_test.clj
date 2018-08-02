;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.local-variable-override-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.testing :refer :all :as ohua-test]
            [ohua.testutils :refer [compat-compile]] [clojure.pprint]))

(ohua :import [ohua.tests])

;;;
;;; NOTE: This works because the variable assignment handling takes one after the other and makes a deep code walk.
;;;       Even though the first round might assign the wrong ID to the innermost target, it will be corrected once
;;;       the innermost let binding is being processed. It will override the previously assigned source-id to be
;;;       the rebound one.
;;;


; TODO Put this back once we have reached a verdict on ohua-core#3
; (deftest variable-rebind-other-let-test
;   "Rebinds the variable in another let closure."
;   ;   (enable-compilation-logging)
;   (let [ohua-code (compat-compile
;                     (let [[one two] (accept "some-id")]
;                       (let [one (read one)] (parse one two)))
;                     :test-compile)]
;     ;     (enable-logging )
;     ;     (write ohua-code :dispatch clojure.pprint/code-dispatch)
;     (test/is
;       (= (count ohua-code)
;          ; init stmt + 3 ops + 3 dependencies + 1 arg + compile stmt
;          9))
;     (test/is
;       (ohua-test/contains
;         ohua-code
;         '((.createOperator "ohua.tests/accept" 100)
;            (.createOperator "ohua.tests/read" 101)
;            (.createOperator "ohua.tests/parse" 102)
;            (.registerDependency 100 0 101 0)
;            (.registerDependency 101 -1 102 0)
;            (.registerDependency 100 1 102 1))))
;     ))

; TODO Same as a above
; (deftest variable-rebind-same-let-test
;   "Rebinds the variable in the same let closure."
;   ;   (enable-compilation-logging)
;   (let [ohua-code (compat-compile
;                     (let [[one two] (accept "some-id")
;                           one (read one)]
;                       (parse one two))
;                     :test-compile)]
;     ;     (enable-logging )
;     ;     (write ohua-code :dispatch clojure.pprint/code-dispatch)
;     (test/is
;       (= (count ohua-code)
;          ; init stmt + 3 ops + 3 dependencies + 1 arg + compile stmt
;          9))
;     (test/is
;       (ohua-test/contains
;         ohua-code
;         '((.createOperator "ohua.tests/accept" 100)
;            (.createOperator "ohua.tests/read" 101)
;            (.createOperator "ohua.tests/parse" 102)
;            (.registerDependency 100 0 101 0)
;            (.registerDependency 101 -1 102 0)
;            (.registerDependency 100 1 102 1))))
;     ))

(deftest var-from-var-assign
  "makes sure that the propagation of the meta data from a variable to a variable works."
  ;(l/enable-compilation-logging)
  (let [ohua-code (compat-compile
                    (let [d (accept 100)
                          x d]
                      (read x)))]

    ;(l/enable-logging)
    ;(l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is (ohua-test/contains ohua-code '((new ohua.lang.compile.FlowGraphCompiler)
                                              (.createOperator "ohua.tests/accept" 100)
                                              (.createOperator "ohua.tests/read" 101)
                                              (.registerDependency 100 -1 101 0)
                                              (.setArguments
                                                100
                                                (clojure.core/into-array
                                                  ohua.lang.Tuple
                                                  (clojure.core/list
                                                    (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.Long))))
                                              (.compile true))))
    ))

(deftest var-from-var-rebind
  ;(l/enable-compilation-logging)
  (let [ohua-code (compat-compile
                    (let [d (accept 100)
                          x d
                          d (read x)]
                      (parse d))
                    :test-compile)]

    ;(l/enable-logging)
    ;(l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is (ohua-test/contains ohua-code '((new ohua.lang.compile.FlowGraphCompiler)
                                              (.createOperator "ohua.tests/accept" 100)
                                              (.createOperator "ohua.tests/read" 101)
                                              (.createOperator "ohua.tests/parse" 102)
                                              (.registerDependency 100 -1 101 0)
                                              (.registerDependency 101 -1 102 0)
                                              (.setArguments
                                                100
                                                (clojure.core/into-array
                                                  ohua.lang.Tuple
                                                  (clojure.core/list
                                                    (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.Long))))
                                              (.compile true))))
    ))

(deftest var-from-var-across-lets
  ;(l/enable-compilation-logging)
  (let [ohua-code (compat-compile
                    (let [d (accept 100)]
                      (let [x d
                            d (read x)]
                        (parse d)))
                    :test-compile)]
    ;(l/enable-logging)
    ;(l/write ohua-code :dispatch clojure.pprint/code-dispatch)
    (test/is (ohua-test/contains ohua-code '((new ohua.lang.compile.FlowGraphCompiler)
                                              (.createOperator "ohua.tests/accept" 100)
                                              (.createOperator "ohua.tests/read" 101)
                                              (.createOperator "ohua.tests/parse" 102)
                                              (.registerDependency 100 -1 101 0)
                                              (.registerDependency 101 -1 102 0)
                                              (.setArguments
                                                100
                                                (clojure.core/into-array
                                                  ohua.lang.Tuple
                                                  (clojure.core/list
                                                    (ohua.lang.Tuple. (clojure.core/int 0) 'java.lang.Long))))
                                              (.compile true))))
    ))
