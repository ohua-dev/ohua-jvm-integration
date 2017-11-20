;;;
;;; Copyright (c) Justus Adam and Sebastian Ertel 2016. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.execution-test
  (:require [clojure.test :refer [deftest]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [ohua.lang :refer [<-ohua ohua]]
            [clojure.test :as test]))

(ohua :import [ohua.tests])

(def ten (int 10))

(deftest single-fn-graph
  ; works due to the extension to capture the result
  (test/is 20
           (<-ohua (add ten 10)))
  ; does not work! the question is: should we support such a think ever??? all the function could ever do is a state change.
  ; (test/is 20
  ;          (ohua (add ten 10)))
  )
