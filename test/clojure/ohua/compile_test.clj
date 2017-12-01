;;;
;;; Copyright (c) Sebastian Ertel 2013. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.compile-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.logging :as l]
            [ohua.link]
            [ohua.testutils :refer [expect-op-arc-count]]))

(ohua-require [ohua.tests :refer :all])

(deftest webserver-with-let
  "testing simple let-based implementation of the web server algorithm"
  []
  (expect-op-arc-count
    (let [r (ohua.tests/accept "port01")]
      (let [s (read r)]
      (let [t (parse s)]
      (let [u (load t)]
      (let [v (write u)]
      (send v))))))
       ; 6 ops + 5 arcs + 1 env arc
    6
    6))

(deftest webserver-nested
  "testing simple function nesting"
  []
  (expect-op-arc-count
    (send (write (load (parse (read (accept "port01"))))))
    ; 6 ops + 5 arcs + 1 env arc
    6
    6))

; reenable once we've got macrosupport again (see lang.clj)
(deftest webserver-threading
  "testing the threading macro"
  []
  (expect-op-arc-count
    (-> "port01" accept read parse load write send)
    ; 6 ops + 5 arcs + 1 env arc
    6
    6))

(deftest output-ports
  "test for output port detection"
  []
  (expect-op-arc-count
    (let [[out-1 out-2] (accept "port01")]
      (read out-1)
      (parse out-2))
       ; 3 ops + 2 arcs + 1 env arc
       ; TODO also verify that the two discovered arcs actually have different targets
    3
    3))

(deftest nested-let
  "test nesting inside let"
  []
  (expect-op-arc-count
    (let [[out-1 out-2] (read (accept "port01"))]
      (parse out-1)
      (load out-2))
    ; 4 ops + 3 arcs + 1 env arc
    ; TODO also verify that the two discovered arcs actually have different targets
    4
    4))

(deftest input-ports-let
  "test for input port detection via let"
  []
  (expect-op-arc-count
    (let [out-1 (accept "port01")
          out-2 (accept "port02")]
      (read out-1 out-2))
    ; 3 ops + 2 arcs + 2 env arcs
    ; TODO also very that the two discovered arcs actually have different targets
    3
    4))

(deftest input-ports-nested
  "test for input port detection in a nested case"
  []
  (expect-op-arc-count
    (read (accept "port01") (accept "port02"))
    ; 3 ops + 2 arcs + 2 env arcs
    ; TODO also very that the two discovered arcs actually have different targets
    3
    4))

(deftest arguments
  "testing the detection of arguments"
  []
  (expect-op-arc-count
    (read (accept "port01") (accept "port02") "arg1" "arg2")
    ; 3 ops + 2 arcs + 4 env arcs
    3
    6))

(deftest arguments-let
  "testing the detection of arguments"
  []
  (expect-op-arc-count
    (let [out-1 (accept "port01")
          out-2 (accept "port02")]
      (read out-1 out-2 "arg1" "arg2"))
    ; 3 ops + 2 arcs + 4 env arcs
    3
    6))

; TODO See issue #11
(deftest arguments-outside
  "testing the detection of arguments"
  []
  (let [an-arg [1 2 3]
        type an-arg]
    (expect-op-arc-count
      (let [out-1 (accept "port01")
            out-2 (accept 9080)]
        (read out-1 out-2 an-arg))
      ; 3 ops + 2 arcs + 3 env arcs
      3
      5)))

; disabled, functions are currently not allowed as arguments. we may chose to trigger on `algo` instead which allows this again
; (deftest arguments-function
;   "testing the detection of arguments"
;   []
;   (l/enable-compilation-logging)
;   (test/is
;     (= (count
;          (ohua (let [out-1 (accept "port01")
;                      out-2 (accept "port02")]
;                  (read out-1 out-2 "arg1" (fn [] [1 2 3]))) :test-compile))
;        ; init stmt + 3 ops + 2 arcs + 3 ops with args + compile stmt
;        10))
;   )

(deftest multiple-arcs-from-one-source
  "testing multiple arcs depending on the same source operator"
  []
  (expect-op-arc-count
    (let [out-1 (accept "port01")
          out-2 (read out-1)]
      (load out-1))
    ; 3 ops + 2 arcs + 1 env arc
    3
    3))
