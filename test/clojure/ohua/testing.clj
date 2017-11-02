;;;
;;; Copyright (c) Sebastian Ertel 2013-2014. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.testing
  (:require [clojure.test :refer :all :as test]
            ;[com.ohua.compile :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint]))

(defn compare-code [actual expected] (= (apply str actual) (apply str expected)))

(def ^:dynamic *compare-meta* false)

(defn compare-deep-code [actual expected]
  (if *compare-meta*
    (let [diffed (diff (dissoc (meta actual) :line :column) (dissoc (meta expected) :line :column))]
      (test/is (not (second diffed))
               (str "Meta comparison: actual >> " (meta actual) " expected >> " (meta expected) " >> Only in expected: " (second diffed)))))
  (cond
    (contains? (meta expected) :skip-comparison) true
    (sequential? actual) (do
                           ;        (println "current actual:" actual)
                           ;        (println "current expected:" expected)
                           (test/is (sequential? expected))
                           (test/is (= (count actual) (count expected))
                                    (str "expected:" expected "\nactual:" actual))
                           (doall (map compare-deep-code actual expected)))
    (record? actual) (do
                       (test/is (record? expected))         ; TODO check for same record type
                       (doall (map compare-deep-code actual expected))
                       )
    :else (test/is (= (str actual) (str expected)))))

(defn pp-code [code] (with-out-str (clojure.pprint/write code :dispatch clojure.pprint/code-dispatch)))

(defn contains
  ([code requirements] (contains code requirements (fn [t] t)))
  ([code requirements comparison-fn]
   (test/is (>= (count code) (count requirements))
            (str "Code:\n" (pp-code code) "\nRequ:\n" (pp-code requirements)))
   (doall
     (map-indexed
       (fn [idx requirement]
         ;        (println "Requirement" requirement)
         ;        (println "Code" code)
         (if
           ; if every item is a sequence then the order doesn't matter
           (and
             (seq? requirement)
             (or (reduce #(and %1 %2) (map seq? requirement))
                 (and (= (str (first requirement)) "clojure.core/doto")
                      (reduce #(and %1 %2) (map seq? (rest requirement))))))
           (contains (nth code idx) requirement)
           (do
             ;               (println (filter #(= % (str requirement)) (map str code)))
             (test/is (comparison-fn (not (empty? (filter #(= % (str requirement)) (map str code)))))
                      (str "Requirement not found:\n"
                           (pp-code requirement)
                           )))))
       requirements))))

(defn filter-special-ops [code ops]
  (let [ops (filter
              #(and
                 (seq? %)
                 (= (nth % 2) (symbol "createOperator"))
                 (some #{(nth % 3)} ops))
              (nth code 2))
        ;_ (println ops)
        op-ids (map #(nth % 4) ops)
        ;_ (println op-ids)
        deps (filter
               #(and
                  (seq? %)
                  (= (nth % 2) (symbol "registerDependency"))
                  (or (some #{(nth % 3)} op-ids)
                      (some #{(nth % 5)} op-ids)))
               (nth code 2))
        ;_ (println deps)
        ]
    [ops deps]
    ))
