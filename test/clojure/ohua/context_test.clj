;;;
;;; Copyright (c) Justus Adam and Sebastian Ertel 2016. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.context-test
  (:require [clojure.test :refer [deftest]]
            ; [ohua.context :as ctxlib]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            ; [ohua.tree-zipper :as t-zip]
            [clojure.test :as test]
            [ohua.testing :refer [compare-deep-code *compare-meta*]]
            [ohua.lang :refer [<-ohua ohua algo]]
            ;[ohua.logging :as l]
            ))

(ohua :import [ohua.tests])

;;;
;;; AST part
;;;
; (deftest ast-annotation
;   (let [test-ast '(let [[d size] ^{:op-id 1} (ohua.lang/smap-io-fun data)]
;                     ^{:op-id 101} (collect ^{:op-id 100} (one-to-n size size)
;                                            (let [inner-size (size d)]
;                                              (let [[f] ^{:op-id 2} (ohua.lang/smap-fun ^{:op-id 103} (one-to-n inner-size d))]
;                                                ^{:op-id 104} (collect ^{:op-id 105} (one-to-n)
;                                                                       (let [[a b c]
;                                                                             ^{:op-id 3} (destruct f)
;                                                                             o
;                                                                             ^{:op-id 4 :switch 20} (if
;                                                                                                      ^{:op-id 5} (check a)
;                                                                                                      ^{:op-id 6} (some-function b)
;                                                                                                      (let [s ^{:op-id 7} (some-other-function c)]
;                                                                                                        ^{:op-id 33 :switch 30} (if
;                                                                                                                                  ^{:op-id 31} (check d)
;                                                                                                                                  ^{:op-id 32} (some-nested-function f))))]
;                                                                         ^{:op-id 8} (ohua.lang/seq
;                                                                                       ^{:op-id 9} (first-this o)
;                                                                                       ^{:op-id 10} (then-that o))))))))



;         [annotated ctxt-map] (t-zip/walk-code (t-zip/tree-zipper test-ast) ctxlib/ctxt-matcher ctxlib/ctxt-editor ctxlib/combine-ctxt-path {})]
;     ;(pprint ctxt-map)
;     (let [diffed (diff {7
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'if, :op-id 20, :out-var 1}],
;                         4
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;                         6
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'if, :op-id 20, :out-var 0}],
;                         3
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;                         2   [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}],
;                         9
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;                         5
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;                         10
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'ohua.lang/seq, :op-id 8, :out-var 0}],
;                         8
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;                         31
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'if, :op-id 20, :out-var 1}],
;                         32
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'if, :op-id 20, :out-var 1}
;                              {:type 'if, :op-id 30, :out-var 0}],
;                         33
;                             [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}
;                              {:type 'ohua.lang/smap-fun, :op-id 2, :out-var 0}
;                              {:type 'if, :op-id 20, :out-var 1}]
;                         104 [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}]
;                         103 [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}]
;                         105 [{:type 'ohua.lang/smap-io-fun, :op-id 1, :out-var 0}]
;                         }
;                        ctxt-map)]
;       (test/is (not (first diffed)))
;       (test/is (not (second diffed))))
;     ;(not (not
;     ;       {7 [nil
;     ;           {:out-var 0, :op-id 2, :type ohua.lang/smap-fun}
;     ;           {:type if, :op-id 20, :out-var 1}],
;     ;        4 [nil
;     ;           {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;     ;        31 [nil
;     ;            {:out-var 0, :op-id 2, :type ohua.lang/smap-fun}
;     ;            {:type if, :op-id 20, :out-var 1}],
;     ;        32 [nil
;     ;            {:out-var 0, :op-id 2, :type ohua.lang/smap-fun}
;     ;            {:out-var 1, :op-id 20} {:type if, :op-id 30, :out-var 0}],
;     ;        33 [nil
;     ;            {:out-var 0, :op-id 2, :type ohua.lang/smap-fun}
;     ;            {:type if, :op-id 20, :out-var 1}],
;     ;        6 [nil
;     ;           {:op-id 2, :type ohua.lang/smap-fun}
;     ;           {:type if, :op-id 20, :out-var 0}],
;     ;        3 [nil
;     ;           {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;     ;        9 [nil {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;     ;        5 [nil {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}],
;     ;        10 [nil {:op-id 2, :type ohua.lang/smap-fun} {:type ohua.lang/seq, :op-id 8, :out-var 0}],
;     ;        8 [nil {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}]}))
;     ;(binding [*compare-meta* true]
;     ;  (test/is
;     ;    (compare-deep-code
;     ;      annotated
;     ;      '^{:op-id 1}
;     ;      (ohua.lang/smap-io-fun
;     ;        (fn [d]
;     ;          ^{:op-id 2,
;     ;            :ctxt
;     ;                   {:type ohua.lang/smap-io-fun, :op-id 1, :out-var 0}}
;     ;          (ohua.lang/smap-fun
;     ;            (fn [s]
;     ;              (let
;     ;                [[a b c]
;     ;                 ^{:op-id 3,
;     ;                   :ctxt
;     ;                          {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}}
;     ;                 (destruct s)
;     ;                 o
;     ;                 ^{:op-id 4,
;     ;                   :ctxt
;     ;                          {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}}
;     ;                 (clojure.core/if
;     ;                   ^{:op-id 5,
;     ;                     :ctxt
;     ;                            {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}}
;     ;                   (check a)
;     ;                   ^{:op-id 6,
;     ;                     :ctxt  {:type clojure.core/if, :op-id 4, :out-var 0}}
;     ;                   (some-function b)
;     ;                   ^{:op-id 7,
;     ;                     :ctxt  {:type clojure.core/if, :op-id 4, :out-var 1}}
;     ;                   (some-other-function c))]
;     ;                ^{:op-id 8,
;     ;                  :ctxt
;     ;                         {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}}
;     ;                (ohua.lang/seq
;     ;                  ^{:op-id 9,
;     ;                    :ctxt
;     ;                           {:type ohua.lang/smap-fun, :op-id 2, :out-var 0}}
;     ;                  (first-this o)
;     ;                  ^{:op-id 10,
;     ;                    :ctxt  {:type ohua.lang/seq, :op-id 8, :out-var 0}}
;     ;                  (then-that o))))
;     ;            d))
;     ;        data)
;     ;      ))
;     )
;   )

; ;;;
; ;;; Dataflow IR part
; ;;;

; (deftest conditionals-transformation
;   ;(some-output
;   ;  (let [x (some-input)]
;   ;    (if x
;   ;      (some-function input)
;   ;      (let [s (some-other-function)]
;   ;        (if x (some-nested-function s))))))

;   (let [graph [(mk-func 1 'some-input [] ['x])
;                (mk-func 2 "ifThenElse" ['x] [])

;                ;true branch
;                (mk-func 3 'some-function ['x] ['r])         ; input from outside of context

;                ;false branch
;                (mk-func 4 'some-other-function [] ['s])     ; function without any data dependency
;                (mk-func 20 "ifThenElse" ['x] [])            ; nested condition with input from outside
;                (mk-func 21 'some-nested-function ['s] ['p]) ; ... and single branch only
;                (mk-func 22 'merge ['p] ['d])

;                (mk-func 5 'merge ['r 'd] ['z])
;                (mk-func 6 'some-output ['z] [])]
;         ctxt-map {3  [{:type 'if, :op-id 2, :out-var 0}]
;                   4  [{:type 'if, :op-id 2, :out-var 1}]
;                   20 [{:type 'if, :op-id 2, :out-var 1}]
;                   21 [{:type 'if, :op-id 2, :out-var 1}
;                       {:type 'if, :op-id 20, :out-var 0}]}
;         transformed (ctxlib/conditionals-ctxt-transformation (->IR graph ctxt-map))]
;     ;(clojure.pprint/pprint transformed)
;     ;(println
;     ;  (meta
;     ;    (first
;     ;      (:args
;     ;        (->IRFunc 3 'some-function [
;     ;                                    ;^:skip-comparison 'true-branch ; for whatever reason, this does not work!
;     ;                                    (with-meta 'true-branch {:skip-comparison true})
;     ;                                    'x] ['r]))
;     ;      )
;     ;    )
;     ;  )
;     (test/is (compare-deep-code (:graph transformed)
;                                 [(mk-func 1 'some-input [] ['x])
;                                  (mk-func 2 "ifThenElse" ['x] [(with-meta 'true-branch {:skip-comparison true}) (with-meta 'false-branch {:skip-comparison true})])
;                                  (mk-func 3 'some-function [(with-meta 'true-branch {:skip-comparison true}) 'x] ['r])
;                                  (mk-func 4 'some-other-function [(with-meta 'false-branch {:skip-comparison true})] ['s])
;                                  (mk-func 20 "ifThenElse" [(with-meta 'true-branch {:skip-comparison true}) 'x] [(with-meta 'true-branch {:skip-comparison true}) (with-meta 'false-branch {:skip-comparison true})])
;                                  (mk-func 21 'some-nested-function [(with-meta 'true-branch {:skip-comparison true}) 's] ['p])
;                                  (mk-func 22 'merge ['p] ['d])
;                                  (mk-func 5 'merge ['r 'd] ['z])
;                                  (mk-func 6 'some-output ['z] [])]
;                                 ))
;     )
;   )

(deftest conditionals-execution
  (test/is (= 5 (<-ohua (if (id true) 5 (fail))))))

(deftest conditionals-algo-execution
  (test/is (= 5 (<-ohua (if (id true) 5 ((algo [] (fail))))))))

(deftest conditionals-algo-with-input-execution
  (test/is (= 5 (<-ohua (if (id true)
                          5
                          ((algo [d] (add (fail) d)) (id 10)))))))

; (deftest seq-transformation
;   ;(some-consumer
;   ;  (seq
;   ;    (some-input)
;   ;    (some-nested-consumer (seq
;   ;                            (some-nested-input)
;   ;                            (some-other-nested-input)))))
;   (let [graph [(mk-func 1 'some-input [] ['x])
;                ; context
;                (mk-func 2 'some-nested-input [] ['r])
;                (mk-func 3 'some-other-nested-input [] ['s])
;                (mk-func 4 'ohua.lang/seq ['r 's] ['t])  ;nested
;                (mk-func 5 'some-nested-consumer ['t] ['y])

;                (mk-func 6 'ohua.lang/seq ['x 'y] ['z])
;                (mk-func 7 'some-consumer ['z] [])]
;         ctxt-map {2 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}]
;                   4 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}]
;                   3 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}
;                      {:type 'ohua.lang/seq :op-id 4 :out-var 0}]}
;         transformed (ctxlib/seq-ctxt-transformation (->IR graph ctxt-map))]
;     ;(clojure.pprint/pprint transformed)
;     (test/is (compare-deep-code (:graph transformed)
;                                 [(mk-func 1 'some-input [] ['x])
;                                  (mk-func 2 'some-nested-input [(with-meta 'seq-ctxt2513 {:skip-comparison true})] ['r])
;                                  (mk-func 3 'some-other-nested-input [(with-meta 'seq-ctxt2512 {:skip-comparison true})] ['s])
;                                  (mk-func 4 'ohua.lang/seq ['r] (with-meta 'seq-ctxt2512 {:skip-comparison true}))
;                                  (mk-func 5 'some-nested-consumer ['s] ['y])
;                                  (mk-func 6 'ohua.lang/seq ['x] (with-meta 'seq-ctxt2513 {:skip-comparison true}))
;                                  (mk-func 7 'some-consumer ['y] [])])
;              )
;     ))

; (deftest directly-nested-seq-transformation
;   "Makes sure that the implementation is realized such that it catches its own changes on the graph."
;   ;(some-consumer
;   ;  (seq
;   ;    (some-input)
;   ;    (seq
;   ;      (some-nested-input)
;   ;      (some-other-nested-input))))
;   (let [graph [(mk-func 1 'some-input [] ['x])
;                ; context
;                (mk-func 2 'some-nested-input [] ['r])
;                (mk-func 3 'some-other-nested-input [] ['s])
;                (mk-func 4 'ohua.lang/seq ['r 's] ['t])  ;nested

;                (mk-func 6 'ohua.lang/seq ['x 't] ['z])
;                (mk-func 7 'some-consumer ['z] [])]
;         ctxt-map {2 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}]
;                   4 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}]
;                   3 [{:type 'ohua.lang/seq :op-id 6 :out-var 0}
;                      {:type 'ohua.lang/seq :op-id 4 :out-var 0}]}
;         transformed (ctxlib/seq-ctxt-transformation (->IR graph ctxt-map))]
;     ;(clojure.pprint/pprint transformed)
;     (test/is (compare-deep-code (:graph transformed)
;                                 [(mk-func 1 'some-input [] ['x])
;                                  (mk-func 2 'some-nested-input [(with-meta 'seq-ctxt2513 {:skip-comparison true})] ['r])
;                                  (mk-func 3 'some-other-nested-input [(with-meta 'seq-ctxt2512 {:skip-comparison true})] ['s])
;                                  (mk-func 4 'ohua.lang/seq ['r] (with-meta 'seq-ctxt2512 {:skip-comparison true}))
;                                  (mk-func 6 'ohua.lang/seq ['x] (with-meta 'seq-ctxt2513 {:skip-comparison true}))
;                                  (mk-func 7 'some-consumer ['s] [])])
;              )
    ))

; (deftest smap-transformation
;   ;(smap
;   ;  (fn []
;   ;    (smap
;   ;      (fn [] (some-nested-fn))
;   ;      (some-fn )))
;   ;  data)
;   (let [graph [(mk-func 1 'ohua.lang/smap-fun ['r] ['x])
;                (mk-func 2 'some-fn [] ['y])
;                (mk-func 3 'ohua.lang/smap-fun ['y] ['z])
;                (mk-func 4 'some-nested-fn [] ['a])
;                (mk-func 5 'ohua.lang/collect ['a] ['b])
;                (mk-func 6 'ohua.lang/collect ['b] ['c])]
;         ctxt-map {2 [{:type 'ohua.lang/smap-fun :op-id 1 :out-var 0}]
;                   3 [{:type 'ohua.lang/smap-fun :op-id 1 :out-var 0}]
;                   4 [{:type 'ohua.lang/smap-fun :op-id 1 :out-var 0}
;                      {:type 'ohua.lang/smap-fun :op-id 3 :out-var 0}]}
;         transformed (ctxlib/smap-ctxt-transformation (->IR graph ctxt-map))]
;     ;(clojure.pprint/pprint transformed)
;     (test/is (compare-deep-code (:graph transformed)
;                                 [(mk-func 1 'ohua.lang/smap-fun ['r] ['x])
;                                  (mk-func 2 'some-fn [(with-meta 'smap-ctxt2526 {:skip-comparison true})] ['y])
;                                  (mk-func 3 'ohua.lang/smap-fun ['y] ['z])
;                                  (mk-func 4 'some-nested-fn [(with-meta 'smap-ctxt2527 {:skip-comparison true})] ['a])
;                                  (mk-func 5 'ohua.lang/collect ['a] ['b])
;                                  (mk-func 6 'ohua.lang/collect ['b] ['c])
;                                  (mk-func nil 'ohua.lang/seq ['x] (with-meta 'smap-ctxt2526 {:skip-comparison true}))
;                                  (mk-func nil, 'ohua.lang/seq ['z] (with-meta 'smap-ctxt2527 {:skip-comparison true}))])
;              )

;     )
;   )

(deftest smap-execution
  (let [data (into () [5 5])
        result (<-ohua (smap (algo [] (id 10)) data))]
    (test/is (= 10 (first result)))
    (test/is (= 10 (second result)))))
(l/enable-compilation-logging)
(deftest smap-with-algo-execution
  (l/enable-logging)
  (let [data (into () [5 5])
        result (<-ohua (smap (algo [d]
                                   (add
                                     (int 20)
                                     ((algo [b] (add (id (int 10)) b)) d)))
                             data))]
    (test/is (= 35 (first result)))
    (test/is (= 35 (second result)))))

;(def testgraph-1
;  [(mk-func 'generator1 [] ['x])
;   (mk-func 'generator2 [] ['y])
;   (mk-func 'ohua.lang/smap ['x 'y] ['z])
;   (mk-func 'f1 ['z] ['b])
;   (mk-func 'ohua.lang/ifThenElse ['b] ['b1 'b2])
;   (mk-func 'f2 [^{:in-idx -1} 'b1 ^{:in-idx 0} 'b] ['c])
;   (mk-func 'f2 [^{:in-idx -1} 'b2 ^{:in-idx 0} 'b] ['c0])
;   (mk-func 'ohua.lang/merge ['c 'c0] ['d])
;   (mk-func 'ohua.lang/collect ['x 'd] ['final])])
;
;(pprint ctxlib/context-trigger-map)
;
;(print "\n\n\n")
;
;(pprint (keys ctxlib/context-trigger-map))
;(pprint (get ctxlib/context-trigger-map 'ohua.lang/ifThenElse))
;
;(pprint (vals (ctxlib/label-graph testgraph-1)))
