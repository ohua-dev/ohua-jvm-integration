;;;
;;; Copyright (c) Sebastian Ertel 2016. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.if-context-function-test
  (:require [clojure.test :as test :refer [deftest is]]
            [clojure.walk :as walk]
            [ohua.testing :as ohua-test]
            [ohua.testutils :as testutils]
            [ohua.util :as util])
  (:use ohua.lang))

(ohua :import [ohua.tests])

(deftest if-executes-context-with-env-args
  (is
    50
    (reduce + 0 (let [data (range 10)]
                  (<-ohua
                    (smap (fn [n]
                            (if (= 1 1)
                              (_const 5)
                              (_const nil)))
                          data))))))

(deftest if-executes-context-without-env-args
  ;(set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) (str "test/if-context-graph-flow"))
  (is
    50
    (reduce + 0 (let [data (range 10)]
                  (<-ohua
                    (smap (fn [n]
                            (if (= 1 1)
                              (const5)
                              (_const nil)))
                          data))))))
