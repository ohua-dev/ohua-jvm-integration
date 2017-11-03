;;;
;;; Copyright (c) Sebastian Ertel 2016. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.if-context-function-test
  (:require [clojure.test :as test :refer [deftest is]]
            [clojure.walk :as walk]
            [ohua.logging :as l]
            [ohua.testing :as ohua-test])
  (:use ohua.lang))

; Try this one after closing #8 and #6

(ohua :import [ohua.lang.tests])

(deftest if-executes-compile
  (let [code '(ohua.lang/ohua
                (ohua.lang/smap (fn [n]
                                      (if (= 1 1) (_const 5) (_const 1)))
                                    [1 2 3 4 5 6 7 8 9 10]))
        compiled (walk/macroexpand-all code)
        [ops deps] (ohua.testing/filter-special-ops compiled '("ifThenElse" "_const"))]
    ;(l/enable-logging)
    (l/write compiled :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code (concat ops deps)
                                 '((. ^:skip-comparison G__2231 createOperator ifThenElse 107)
                                    (. ^:skip-comparison G__2231 createOperator _const 109)
                                    (. ^:skip-comparison G__2231 createOperator _const 110)
                                    (. ^:skip-comparison G__2231 registerDependency 109 -1 106 0)
                                    (. ^:skip-comparison G__2231 registerDependency 110 -1 106 1)
                                    (. ^:skip-comparison G__2231 registerDependency 107 0 109 -1)
                                    (. ^:skip-comparison G__2231 registerDependency 107 1 110 -1)))
    ))

(deftest support-for-fns-without-args
  (let [code '(ohua.lang/ohua
                (ohua.lang/smap (fn [n]
                                      (if (= 1 1) (const5) (const1)))
                                    [1 2 3 4 5 6 7 8 9 10]))
        compiled (walk/macroexpand-all code)
        [ops deps] (ohua.testing/filter-special-ops compiled '("ifThenElse" "const5" "const1"))]
    ;(l/enable-logging)
    (l/write compiled :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code (concat ops deps)
                                 '((. ^:skip-comparison G__2284 createOperator ifThenElse 107)
                                    (. ^:skip-comparison G__2284 createOperator const5 109)
                                    (. ^:skip-comparison G__2284 createOperator const1 110)
                                    (. ^:skip-comparison G__2284 registerDependency 109 -1 106 0)
                                    (. ^:skip-comparison G__2284 registerDependency 110 -1 106 1)
                                    (. ^:skip-comparison G__2284 registerDependency 107 0 109 -1)
                                    (. ^:skip-comparison G__2284 registerDependency 107 1 110 -1)))
    ))

(deftest if-executes-context-with-env-args
  (is
    50
    (reduce + 0 (let [data (range 10)]
                  (<-ohua
                    (smap (fn [n]
                            (if (= 1 1)
                              (_const 5)))
                          data))))))

(deftest if-executes-context-without-env-args
  ;(set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/if-context-graph-flow"))
  (is
    50
    (reduce + 0 (let [data (range 10)]
                  (<-ohua
                    (smap (fn [n]
                            (if (= 1 1)
                              (const5)))
                          data))))))
