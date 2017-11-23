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
            [ohua.testing :refer :all :as ohua-test]))

(ohua :import [ohua.tests])

; Make this work after closing issues #6 and #4

; FIXME this does not work yet because we do not allow to mix static with dynamic arguments yet.
;       but in this case the first arg, the function reference, would be static!
; FIXME fix this after #7
; (deftest apply-run-on-static-func-ref
;  "Tests the apply functionality at runtime on a pre-defined function."
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce)] (apply consume prod result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )

; FIXME see issue #160
; FIXME Needs #7
; (deftest apply-run-on-dynamic-func-ref
;  "Tests the apply functionality at runtime on a function object."
;  ;  (ohua (let [prod (produce)]
;  ;            (if (< prod 3) (consume prod if-result) (consume prod else-result))))
;  (let [result (int-array [0])]
;    (ohua (let [[prod consume-fn] (produceFn)] (apply consume-fn prod result)))
; ;    (println result)
;    (test/is (= (first result) 111)))
;  )


; FIXME currently these fail
; (deftest partial-run
;  "Tests the partial functionality at runtime."
;  (l/enable-compilation-logging )
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce )
;                consume-fn (partial consume prod)] (apply consume-fn result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )

; Same as above but with a clojure function
(deftest partial-run
 "Tests the partial functionality at runtime."
 (let [result (atom nil)
       local-consume (fn [value var] (reset! var value))]
   (ohua (let [prod (produce )
               consume-fn (clojure.core/partial local-consume prod)] 
      (apply consume-fn result)))
   (println @result)
   (test/is (= @result 111)))
 )

; (deftest partial-call-run
;  "Tests the partial functionality at runtime but calls the function directly."
;  (let [result (int-array [0])]
;    (ohua (let [prod (produce )
;                consume-fn (partial consume prod)] (consume-fn result)))
;    (println result)
;    (test/is (= (first result) 9)))
;  )
