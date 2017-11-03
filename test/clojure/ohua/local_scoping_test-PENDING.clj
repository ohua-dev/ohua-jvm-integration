;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.local-scoping-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer [ohua <-ohua algo]]
            [ohua.compile :refer [transformation]]
            [clojure.pprint :refer :all]
            [clojure.string :refer [join]]
            [ohua.transform.local-variable-scoping :refer [transform]]
            [ohua.logging :as l]
            [ohua.testing :refer :all :as ohua-test]
            [clojure.walk :as walk]))

(ohua :import [ohua.tests])

; try this once #10 is done

(deftest local-scopes-in-clojure
  "For now this is a simple test for scopes of locals."
  []
  ; the inner-most closure redefines 'a'.
  (let [a "something"]
    (let [a (join " " [a "else"])]
      (test/is a "something else"))))

(deftest simple-test
  "Simple first test for a rewrite."
  ;  (l/enable-logging )
  (let [t-code (transform '(if (< 1 2) (some-func2 var2 (some-func1 var1 (top)))))]
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1] (scope var2 var1)]
                                             (some-func2 var2 (some-func1 var1 (top))))))))

(deftest top-locals-optimization-no-scope-test
  "Checks to see if the optimization for locals input to top-level functions works."
  ;  (l/enable-logging )
  (let [t-code (transform '(if (< 1 2) (some-func1 (top var1))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (some-func1 (top var1))))))

(deftest top-locals-optimization-reduced-scope-test
  "Checks to see if the optimization for locals input to top-level functions works."
  ; (l/enable-logging )
  (let [t-code (transform '(if (< 1 2) (some-func1 var2 (top var1))))]
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2] (scope var2)]
                                             (some-func1 var2 (top var1)))))))

(deftest top-locals-optimization-nested-scope-test
  "Makes sure that the optimization only works on the top scope."
  (let [t-code (transform '(if (< 1 2)
                             (let [var0 (some-func1 var1)]
                               (if (< 3 4) (some-func3 var3)))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var3] (scope var3)]
                                             (let [var0 (some-func1 var1)]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var3] (scope var3)]
                                                   (some-func3 var3)))))))))


(deftest no-if-test
  "Makes sure that the transformation code passes nicely even when no condition is present."
  (let [t-code (transform '(some-func2 var2 (some-func1 var1)))]
    (test/is (ohua-test/compare-code t-code '(some-func2 var2 (some-func1 var1))))))

(deftest if-else-test
  "Test with else branch."
  (let [t-code (transform '(if (< 1 2) (some-func2 var2 (some-func1 var1 (top))) (some-func4 var4 (some-func3 var3 (top)))))]
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1] (scope var2 var1)]
                                             (some-func2 var2 (some-func1 var1 (top))))
                                           (clojure.core/let [[var4 var3] (scope var4 var3)]
                                             (some-func4 var4 (some-func3 var3 (top))))
                                           ))))

(deftest inner-lvars-test
  "Local variables that are defined inside the branch, do not need to be scoped."
  (let [t-code (transform '(if (< 1 2)
                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                               (some-func3 var3 var4))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1] (scope var2 var1)]
                                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                                               (some-func3 var3 var4))))))

  )

(deftest nested-if-test
  "Nested if statement."
  (let [t-code (transform '(if (< 1 2)
                             (let [var0 (some-func2 var2 (some-func1 var1 (top)))]
                               (if (< 3 4) (some-func4 var4 (some-func3 var3))))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1 var4 var3] (scope var2 var1 var4 var3)]
                                             (let [var0 (some-func2 var2 (some-func1 var1 (top)))]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var4 var3] (scope var4 var3)]
                                                   (some-func4 var4 (some-func3 var3))))))))))

(deftest nested-if-prop-test
  "Nested if statement."
  (let [t-code (transform '(if (< 1 2)
                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                               (if (< 3 4) (some-func4 var4 (some-func3 var3))))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1] (scope var2 var1)]
                                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var4 var3] (scope var4 var3)]
                                                   (some-func4 var4 (some-func3 var3))))))))))

(deftest nested-if-else-test
  "Nested else branch."
  ;  (l/enable-logging )
  (let [t-code (transform '(if (< 1 2)
                             (let [var0 (some-func2 var2 (some-func1 var1 (top)))]
                               (if (< 3 4)
                                 (some-func4 var4 (some-func3 var3))
                                 (some-func6 var6 (some-func5 var5))))))]
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1 var4 var3 var6 var5] (scope var2 var1 var4 var3 var6 var5)]
                                             (let [var0 (some-func2 var2 (some-func1 var1 (top)))]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var4 var3] (scope var4 var3)]
                                                   (some-func4 var4 (some-func3 var3)))
                                                 (clojure.core/let [[var6 var5] (scope var6 var5)]
                                                   (some-func6 var6 (some-func5 var5))))))))
    ))


(deftest nested-if-else-prop-test
  "Nested else branch with stopped propagation of locals used on the innermost if-branch."
  ;  (l/enable-logging )
  (let [t-code (transform '(if (< 1 2)
                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                               (if (< 3 4)
                                 (some-func4 var4 (some-func3 var3))
                                 (some-func6 var6 (some-func5 var5))))))]
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1 var6 var5] (scope var2 var1 var6 var5)]
                                             (let [[var3 var4] (some-func2 var2 (some-func1 var1 (top)))]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var4 var3] (scope var4 var3)]
                                                   (some-func4 var4 (some-func3 var3)))
                                                 (clojure.core/let [[var6 var5] (scope var6 var5)]
                                                   (some-func6 var6 (some-func5 var5))))))))
    ))

(deftest propagation-test
  "Even though the outer if does not use the locals they are still coming from outside so we need to propate them."
  (let [t-code (transform '(if (< 1 2)
                             (let [[var3 var4] (some-func2 (some-func1))]
                               (if (< 3 4) (some-func4 var2 (some-func3 var1))))))]
    ;      (l/enable-logging )
    ;      (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (clojure.core/let [[var2 var1] (scope var2 var1)]
                                             (let [[var3 var4] (some-func2 (some-func1))]
                                               (if (< 3 4)
                                                 (clojure.core/let [[var2 var1] (scope var2 var1)]
                                                   (some-func4 var2 (some-func3 var1))))))))
    ))

(deftest propagation-stop-test
  "Here, there is a rewrite happening at a lower level but the top if-condition does not require a rewrite.
   However, the code still needs to do so in order to carry over the nested rewrite!"
  (let [t-code (transform '(if (< 1 2)
                             (let [[var3 var4] (some-func2 (some-func1))]
                               (if (< 3 4) (some-func4 var4 (some-func3 var3))))))]
    ;     (l/enable-logging )
    ;     (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(if (< 1 2)
                                           (let [[var3 var4] (some-func2 (some-func1))]
                                             (if (< 3 4)
                                               (clojure.core/let [[var4] (scope var4)]
                                                 (some-func4 var4 (some-func3 var3))))))))
  )

(deftest cond-test
  "Tests the expansion of the 'cond' macro into a nested if-statement."
  (let [t-code (transformation '(let [[var3 var4] (some-func2 (some-func1))]
                                  (cond (< 3 4) (some-func4 var4 (some-func3 var3))
                                        (< 5 6) (some-func6 var4 (some-func5 var3))))
                               {:skip-symbol-resolution true})]
    ;   (l/enable-logging )
    ;   (l/printline "Transformed code:")
    ;   (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (ohua-test/compare-deep-code t-code '(let*
                                           [^:skip-comparison vec__962 (some-func2 (some-func1))
                                            var3 (clojure.core/nth ^:skip-comparison vec__962 0 nil)
                                            var4 (clojure.core/nth ^:skip-comparison vec__962 1 nil)]
                                           (if (clojure.core/boolean (< 3 4))
                                             (clojure.core/let [[var4] (scope var4)]
                                               (some-func4 var4 (some-func3 var3)))
                                             (clojure.core/let [[var4 var3] (scope var4 var3)]
                                               (if (clojure.core/boolean (< 5 6))
                                                 (clojure.core/let [[var4 var3] (scope var4 var3)]
                                                   (some-func6 var4 (some-func5 var3)))
                                                 (ohua.lang/id nil)))))))
  )

(deftest compilation-integration
  "Compiles a flow graph and makes sure the transformation is integrated well."
  ;  (l/enable-compilation-logging )
  (let [
        ;          t-code (transform '(let [[var1 var2 var3 var4] (read (accept 80))]
        ;                               (if (< 3 4)
        ;                                 (send var2 (parse var1))
        ;                                 (send var4 (parse var3)))))
        ohua-code (ohua
                    (let [[var1 var2 var3 var4] (read (accept 80))]
                      (if (< 3 4)
                        (send var2 (parse var1))
                        (send var4 (parse var3))))
                    :test-compile)
        ]
    ;     (l/enable-logging )
    ;     (l/write t-code :dispatch clojure.pprint/code-dispatch)
    ;     (l/printline)
    ;     (l/write ohua-code :dispatch clojure.pprint/code-dispatch)

    (ohua-test/contains
      ohua-code
      '((.createOperator "ohua.tests/read" 100)
         (.createOperator "ohua.lang/scope" 105)
         (.createOperator "ohua.tests/send" 106)
         (.createOperator "ohua.tests/parse" 107)
         (.createOperator "ohua.tests/send" 109)
         (.createOperator "ohua.tests/parse" 110)
         ; var1
         (.registerDependency 100 0 107 0)
         ; var2
         (.registerDependency 100 1 105 0)
         (.registerDependency 105 0 106 0)
         ; var3
         (.registerDependency 100 2 110 0)
         ; var4
         (.registerDependency 100 3 108 0)
         (.registerDependency 108 0 109 0)))
    )
  )

(deftest runtime-integration
  "Runs a small program and makes sure that the scope function is executed properly."
  ;  (set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) "../test-output/runtime-integration-flow")
  (let [input (map int (range 10))
        result (<-ohua
                 (smap
                   (fn [prod]
                     (if (< prod 3)
                       (add prod 100)
                       (if (< prod 5) (add prod 200) (subtract prod 3))))
                   input))]
    (test/is
      (= (reduce + result) 730)))
  )

(deftest scope-function-compoundness
  "Runs a small program and where the scope function has more than one argument. 
   This test makes sure that the code for handling compound argument lists is working properly."
  ;  (l/enable-compilation-logging )
  ;  (set! (. ohua.engine.utils.GraphVisualizer PRINT_FLOW_GRAPH) "../test-output/runtime-integration-flow")
  (let [input (map int (range 10))
        result (<-ohua
                 (smap
                   (fn [p]
                     (let [prod (peek p) prod2 (peek p)]
                       (if (< prod 3)
                         (add prod 100)
                         (if (< prod2 5) (add prod2 200) (subtract prod 3)))
                       ))
                   input))]
    (test/is
      (= (reduce + result) 730)))
  )

(defn some-algo [some-fn-arg]
  (def some-var '())
  (ohua (let [prod (produce)
              result '()]
          (collect (if (< prod 8)
                     (add prod 2300)
                     (if (< prod 10) (add prod 400 some-var) (subtract prod 4 some-fn-arg)))
                   result))
        :test-compile))

(deftest variables-outside-ohua
  "Make sure that you do not collect any environment variables!"
  (let [t-code (some-algo 10)]
    ;     (l/enable-logging )
    ;     (l/write t-code :dispatch clojure.pprint/code-dispatch)
    (let [scope-ids (keep #(if (and (= (first %) '.createOperator) (= (second %) "scope")) (second (rest %))) t-code)
          args-to-scope (keep #(if (and (= (first %) '.setArgument) (some #{(second %)} scope-ids)) %) t-code)]
      (test/is (empty? args-to-scope)))
    ))

(deftest local-name-overshadowing
  "Here, 'key' is defined as a local variable. However, our algorithm for scoping local variables does not identify it as such."
  (let [data (into () [(into-array Object (list 30 (list (int 100))))
                       (into-array Object (list 40 (list (int 200))))])
        result (<-ohua
                 (smap
                   (algo [ [key b] ]
                         (if (< 5 10)
                           (id key)
                           (smap (algo [c] (add key c)) b)))
                   data))]
    (test/is 30 (first result))
    (test/is 40 (second result))))
