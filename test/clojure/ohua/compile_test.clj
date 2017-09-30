;;;
;;; Copyright (c) Sebastian Ertel 2013. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.compile-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.logging :as l]))

(ohua :import [com.ohua.lang.tests])

(deftest webserver-with-let
  "testing simple let-based implementation of the web server algorithm"
  []
  (test/is
    (= (count
         (ohua
          (let [r (accept "port01")]
            (let [s (read r)]
            (let [t (parse s)]
            (let [u (load t)]
            (let [v (write u)]
            (send v))))))
          :test))
       ; init stmt + 6 ops + 5 arcs + 1 op with arg + exec stmt
       14)))

(deftest webserver-nested
  "testing simple function nesting"
  []
  (test/is
    (= (count
         (ohua (send (write (load (parse (read (accept "port01")))))) :test))
         ; 6 ops + 5 arcs + 1 op with arg + exec stmt
       14)))

(deftest webserver-threading
  "testing the threading macro"
  []
  (test/is
    (= (count
         (ohua (-> "port01" accept read parse load write send) :test))
         ; init stmt + 6 ops + 5 arcs + 1 op with arg + exec stmt
         14)))

(deftest output-ports
  "test for output port detection"
  []
  (test/is
    (= (count
         (ohua (let [[out-1 out-2] (accept "port01")]
                 (read out-1)
                 (parse out-2)) :test))
       ; init stmt + 3 ops + 2 arcs + 1 op with arg + exec stmt
       ; TODO also very that the two discovered arcs actually have different targets
       8)))

(deftest nested-let
  "test nesting inside let"
  []
  (test/is
    (= (count
         (ohua (let [[out-1 out-2] (read (accept "port01"))]
                 (parse out-1)
                 (load out-2)) :test))
       ; init stmt + 4 ops + 3 arcs + 1 op with arg + exec stmt
       ; TODO also verify that the two discovered arcs actually have different targets
       10)))

(deftest input-ports-let
  "test for input port detection via let"
  []
  (test/is
    (= (count
         (ohua (let [out-1 (accept "port01")
                     out-2 (accept "port02")]
                 (read out-1 out-2)) :test))
       ; init stmt + 3 ops + 2 arcs + 2 ops with arg + exec stmt
       ; TODO also very that the two discovered arcs actually have different targets
       9)))

(deftest input-ports-nested
  "test for input port detection in a nested case"
  []
  (test/is
    (= (count
         (ohua (read (accept "port01") (accept "port02")) :test))
       ; init stmt + 3 ops + 2 arcs + 2 ops with arg + exec stmt
       ; TODO also very that the two discovered arcs actually have different targets
       9)))

(deftest arguments
  "testing the detection of arguments"
  []
  (test/is
    (= (count
         (ohua (read (accept "port01") (accept "port02") "arg1" "arg2") :test-compile))
       ; init stmt + 3 ops + 2 arcs + 3 ops with args + exec stmt
       10)
    ))

(deftest arguments-let
  "testing the detection of arguments"
  []
  (test/is
    (= (count
         (ohua (let [out-1 (accept "port01")
                     out-2 (accept "port02")]
                 (read out-1 out-2 "arg1" "arg2")) :test-compile))
       ; init stmt + 3 ops + 2 arcs + 3 ops with args + exec stmt
       10)
    ))

(deftest arguments-outside
  "testing the detection of arguments"
  []
  (let [an-arg [1 2 3]
        type an-arg
        _ (clojure.pprint/pprint "debug: ")]
    (test/is
      (= (count
           (ohua (let [out-1 (accept "port01")
                       out-2 (accept 9080)]
                   (read out-1 out-2 an-arg)) :test-compile))
         ; init stmt + 3 ops + 2 arcs + 3 ops with args + exec stmt
         10))
      ))

(deftest arguments-function
  "testing the detection of arguments"
  []
  (l/enable-compilation-logging)
  (test/is
    (= (count
         (ohua (let [out-1 (accept "port01")
                     out-2 (accept "port02")]
                 (read out-1 out-2 "arg1" (fn [] [1 2 3]))) :test-compile))
       ; init stmt + 3 ops + 2 arcs + 3 ops with args + compile stmt
       10))
  )

(deftest multiple-arcs-from-one-source
  "testing multiple arcs depending on the same source operator"
  []
  (test/is
    (= (count
         (ohua (let [out-1 (accept "port01")
                     out-2 (read out-1)]
                 (load out-1)) :test-compile))
       ; init stmt + 3 ops + 2 arcs + 1 ops with args + compile stmt
       8))
  )
