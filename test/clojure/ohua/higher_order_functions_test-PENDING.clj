;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.higher-order-functions-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer [ohua]]
            [clojure.pprint :refer :all]
            [clojure.string :refer [join]]
            [ohua.transform.higher-order-functions :refer [transform]]
            [ohua.logging :as l]
            [ohua.testing :refer :all :as ohua-test]
            [ohua.walk :as walk]))

(ohua :import [ohua.lang.tests])

; Make this work after closing issues #6 and #4

(deftest apply-rewrite
  "Tests the transformation algorithm."
;  (l/enable-logging )
  (let [t-code (transform (walk/macroexpand-all '(let [some-func (produce "some-arg")] (some-func 500))))]
;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
      (ohua-test/compare-deep-code t-code '(let* [some-func (produce "some-arg")] (apply some-func 500)))))

; FIXME this does not work yet because we do not allow to mix static with dynamic arguments yet. 
;       but in this case the first arg, the function reference, would be static!
;(deftest apply-run-on-static-func-ref
;  "Tests the apply functionality at runtime on a pre-defined function."
;  (l/enable-logging )
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce)] (apply consume prod result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )

; FIXME see issue #160
;(deftest apply-run-on-dynamic-func-ref
;  "Tests the apply functionality at runtime on a function object."
;  ;  (ohua (let [prod (produce)]
;  ;            (if (< prod 3) (consume prod if-result) (consume prod else-result))))
;;  (l/enable-logging )
;  (let [result (int-array [0])]
;    (ohua (let [[prod consume-fn] (produce-fn)] (apply consume-fn prod result)))
;;    (println result)
;    (test/is (= (first result) 111)))
;  )

(deftest partial-fn-rewrite
  "Tests the transformation algorithm for the partial higher-order function when the argument is a function reference."
;  (l/enable-logging )
  (let [t-code (transform (walk/macroexpand-all '(let [partial-func (partial produce "some-arg")] (collect 500))))]
;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
      (ohua-test/compare-deep-code t-code '(let* [partial-func (partial (ohua.link/resolve-and-init produce) "some-arg")] 
                                             (collect 500)))))

(deftest partial-fn-rewrite-var-input
  "Tests the transformation algorithm for the partial higher-order function when the argument is a function (local variable)."
;  (l/enable-logging )
  (let [t-code (transform (walk/macroexpand-all '(let [some-func (produce "some-arg") 
                                                       partial-func (partial some-func "another-arg")] (collect 500))))]
;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
      (ohua-test/compare-deep-code t-code '(let* [some-func (produce "some-arg") 
                                                        partial-func (partial some-func "another-arg")] (collect 500)))))

(deftest partial-fn-rewrite-direct-input
  "Tests the transformation algorithm for the partial higher-order function when the argument is a function (direct passing)."
;  (l/enable-logging )
  (let [t-code (transform (walk/macroexpand-all '(let [partial-func (partial (produce "some-arg") "another-arg")] (collect 500))))]
;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
      (ohua-test/compare-deep-code t-code '(let* [partial-func (partial (produce "some-arg") "another-arg")] (collect 500)))))

; FIXME currently these fail
;(deftest partial-run 
;  "Tests the partial functionality at runtime."
;  (l/enable-compilation-logging )
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce )
;                consume-fn (partial consume prod)] (apply consume-fn result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )
;
;(deftest partial-call-run 
;  "Tests the partial functionality at runtime but calls the function directly."
;  (l/enable-compilation-logging )
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce )
;                consume-fn (partial consume prod)] (consume-fn result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )
