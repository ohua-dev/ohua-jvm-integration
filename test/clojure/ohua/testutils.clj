(ns ohua.testutils
  (:require [ohua.lang :as lang]
            [ohua.util :refer [+option macroexpand-all report-option-type]]
            [ohua.link]
            [clojure.pprint])
  (:import (ohua.graph Source$Env Source$Local)))


(defn is-local-arc? [arc]
  (instance? Source$Local (.source arc)))


(defn is-env-arc? [arc]
  (instance? Source$Env (.source arc)))

(defn- adjust-op-id [id] 
  (+ 99 id))

(defn ptrace [a]
  (clojure.pprint/pprint a)
  a)

(defmacro compat-compile
  "NOTICE: This is only for compatibility with old tests. 
   This does not correctly evaluate env arguments 
   (they are not evaluated in their scope). 
   Do not use this to create production code. "
  ([code] `(ohua ~code {}))
  ([code option0] 
    (let [option (+option option0 :test-compile)
          ; TODO do not eval env arcs
          gr (ohua.Compiler/compileAndSpliceEnv
                  ohua.link/clj-linker
                  (ptrace (macroexpand-all code)))
          ops (map
                (fn [op] `(.createOperator ~(name (symbol (.type op))) ~(adjust-op-id (.id op))))
                (sort-by #(.id %) (.operators gr)))
          arcs (map
                (fn [arc] 
                  (let [src (.target (.source arc))]
                    `(.registerDependency 
                        ~(adjust-op-id (.operator src))
                        ~(.index src) 
                        ~(adjust-op-id (.operator (.target arc)))
                        ~(.index (.target arc)))))
                (sort-by #(.operator (.target %)) (filter is-local-arc? (.arcs gr))))
          env-arcs (filter is-env-arc? (.arcs gr))
          env-target-seq (distinct (map #(.operator (.target %)) env-arcs))
          op-sorted-env-arcs (group-by #(.operator (.target %)) env-arcs)
          env-arcs (map
                    (fn [target-op]
                      (let [args (op-sorted-env-arcs target-op)]
                        (assert (not (nil? args)))
                        `(.setArguments ~(adjust-op-id target-op)
                            (into-array 
                              com.ohua.lang.Tuple
                                ~(cons 'list
                                  (map 
                                    (fn [arc]
                                      `(com.ohua.lang.Tuple. (int ~(.index (.target arc))) (quote ~(type (.hostExpr (.source arc))))))
                                    args))))))
                    env-target-seq)
          ]
      `'(
        (new com.ohua.lang.compile.FlowGraphCompiler)
        ~@ops
        ~@arcs
        ~@env-arcs
        (.compile true)))))
