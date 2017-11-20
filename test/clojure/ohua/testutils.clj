(ns ohua.testutils
  (:require [ohua.lang :as lang]
            [ohua.util :refer [+option macroexpand-all report-option-type]]
            [ohua.link]
            [clojure.pprint])
  (:import (ohua.graph Source$Env Source$Local)
           (ohua.lang Condition)))


(defn is-local-arc? [arc]
  (instance? Source$Local (.source arc)))


(defn is-env-arc? [arc]
  (instance? Source$Env (.source arc)))

(defn- adjust-op-id [id]
  (+ 99 id))

(defn ptrace [a]
  (clojure.pprint/pprint a)
  a)

(defn get-type-or-throw [source]
  (let [lazy (.hostExpr source)]
    (if (.isRealized lazy)
      (type (.get lazy))
      (throw (Exception. "Cannot evaluate local arguments in compat-compile")))))

(defmacro compat-compile
  "NOTICE: This is only for compatibility with old tests.
   This does not correctly evaluate env arguments
   (they are not evaluated in their scope).
   Do not use this to create production code. "
  ([code] `(compat-compile   ~code {}))
  ([code option0]
    (let [option (+option option0 :test-compile)
          ; TODO do not eval env arcs
          gr (ohua.Compiler/compileAndSpliceEnv
                  (first (ohua.link/clj-linker))
                  (ptrace (macroexpand-all code)))
          rename-op-types (if (:strip-ns option) #(name (symbol %)) identity)
          ops (map
                (fn [op] `(.createOperator ~(rename-op-types(.type op)) ~(adjust-op-id (.id op))))
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
          env-arcs
          (map
            (fn [target-op]
              (let [args (op-sorted-env-arcs target-op)]
                (assert (not (nil? args)))
                `(.setArguments ~(adjust-op-id target-op)
                    (into-array
                      ohua.lang.Tuple
                        (list
                          ~@(map
                              (fn [arc]
                                `(ohua.lang.Tuple. (int ~(.index (.target arc))) (quote ~(get-type-or-throw (.source arc)))))
                              args))))))
            env-target-seq)
          ]
      `'(
        (new ohua.lang.compile.FlowGraphCompiler)
        ~@ops
        ~@arcs
        ~@env-arcs
        (.compile true)))))


(defmacro expect-op-arc-count [code ops arcs]
  `(let [gr# (ohua.lang/ohua ~code :test-compile)]
    (test/is (= ~ops (count (.operators gr#))))
    (test/is (= ~arcs (count (.arcs gr#))))))


(defn mk-cond [thing]
  (reify Condition
    (check [_ _] (boolean thing))))
