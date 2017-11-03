;;;
;;; Copyright (c) Sebastian Ertel 2013. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.explicit-schema-match-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.testing :as ohua-test]
            [ohua.testutils :refer [expect-op-arc-count]]))

(ohua :import [ohua.tests])

(deftest simple-match
  "testing the case where output has to be disptached to multiple downstream ops (and skip over one)"
  []
  (expect-op-arc-count
    (let [[one two _] (accept "input")]
      (read one)
      (write two))
    3
    3
    ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
    ))

(deftest switch-order-match
 "values are used in reverse order"
  []
  (expect-op-arc-count
    (let [[one two _] (accept "input")]
      (read two)
      (write one))
    3
    3
        ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
    ))

(deftest multiple-to-one
  "multiple values arcs are passed to the same operator -> should result into a single arc only"
  []
  (expect-op-arc-count
    (let [[one two three] (accept "input")]
      (read two)
      (write one three))
      ; init stmt + 3 ops + 3 dependencies + 1 arg + compile stmt
    3
    4))

(deftest mixed
  "a function that takes multiple partial outputs from one op and an implicit from another"
  []
  (expect-op-arc-count
    (let [[one two three] (accept "input")]
      (write one (read "something") three))
        ; init stmt + 3 ops + 3 dependencies + 2 arg + compile stmt
    3
    5))

(deftest mixed-twisted
 "like the above but switches again the partial outputs"
  []
  (expect-op-arc-count
    (let [[one two three] (accept "input")]
      (write three (read "something") one))
        ; init stmt + 3 ops + 3 dependencies + 2 arg + compile stmt
    3
    5))

; Not sure how this should be handeled ... but we can add it back once we have clojure function embedding
; (deftest direct-access
;  "accessing an output argument list via indexing."
;  []
;  (expect-op-arc-count
;     (let [packet (accept "input")]
;       (read (nth packet 0))
;       (write (nth packet 1)))
;       ; init stmt + 3 ops + 2 dependencies + 1 arg + compile stmt
;    3
;    3))
