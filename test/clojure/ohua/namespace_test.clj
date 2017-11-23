;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.namespace-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.testing :refer :all :as ohua-test]
            [clojure.pprint]))

(ohua :import [ohua.tests])

; Again issue #10

(defn mytrace [msg a] (print msg) (clojure.pprint/pprint a) a)



(deftest test-ns-frontend-support
  "This test just makes sure that the Clojure frontend does not choke on namespaces
   even though they might not be supported properly yet."

  ;  (l/enable-compilation-logging )
  (let [nsp (create-ns 'ohua.tests)]
    (println nsp)
    (let [input (map int (range 10))
          _ (println "input" input)
          result (<-ohua
                   (smap
                     (fn [prod_]
                        (let [prod (mytrace "array" prod_)]
                       (mytrace "after if"
                          (if (< prod 3)
                            (ohua.tests/add (mytrace "delay" (mytrace "delay" prod)) 100)
                            (ohua.tests/subtract prod 3)))))
                     input))]
      ;      (ohua (let [prod (produce)]
      ;              (collect (if (< prod 3) (add prod 100) (subtract prod 3)) result)))
      (test/is
        (= (reduce + result) 324))))
  )
