(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]
            [ohua.util :refer [macroexpand-all report-option-type +option]])
  (:import java.util.concurrent.atomic.AtomicReference))

(defn mk-hs-compile-options [opts]
  (if (:logging opts)
    (if (set? opts)
      (throw (Exception. "Options must be map to set the logging level!"))
      (:logging opts))
    :warning))

(defmacro algo
  "Compile an algo. This produces alang as output.
   However the advantage here is that as this compiles the algo all sf-refs should be absolute
   in the result. Also all other algos should be inlined already. This means this approach
   (in contrast to the old one) respects the scoping of the linker."
  [args & code]
  (let [[linker gen-init-code] (ohua.link/clj-linker)
        a (ohua.Compiler/compileAlgo
            (mk-hs-compile-options {})
            linker
            (cons 'algo
              (cons args code)))
        a-name (gensym "algo")]
    (intern *ns* a-name a)
    `(do
      ~(gen-init-code)
      ~a-name)))


(defmacro defalgo
  "I am rather happy with the new relationship between `algo` and `defalgo` since
   `(defalgo name [args] code)` is now literally the same as `(def name (algo [args] code))`."
  [name & code]
  `(def ~name (algo ~@code)))


(defn ohua-fn
  "Compile the code, create the graph, prepare the runtime and save the executable with a global, generated name.
   Returns code that invokes the saved executable when evaluated."
  [code option_]
  (if (= :import code)
    (do
      (println "Using the :import version of the ohua macro is deprecated")
      (apply ohua.link/ohua-require-fn (map (fn [ns_] [ns_ :refer :all]) option_)))
    (let [_ (ohua.link/get-linker)
          option (cond
                   (or (set? option_) (map? option_)) option_
                   (keyword? option_) #{option_}
                   :else (report-option-type option_))
          mk-qual (fn [thing] (if (namespace thing) thing (symbol (str *ns*) (name thing))))
          register #(intern *ns* %1 %2)
          [code-with-capture final-code-modifier]
          (if (option :capture)
            (let [ref (AtomicReference.)
                  ref-sym (gensym "capture-ref")
                  qual-ref-sym (mk-qual ref-sym)]
              (register ref-sym ref)
              [`(ohua.lang/capture ~code ~qual-ref-sym)
               (fn [run] `(do ~run (.get ~qual-ref-sym)))])
            [code identity])
          [linker gen-init-code] (ohua.link/clj-linker)
          graph (ohua.Compiler/compileAndSpliceEnv
                  (mk-hs-compile-options option)
                  linker
                  (macroexpand-all code-with-capture))]
      (if (option :test-compile)
        (let [gr-sym (gensym "graph")]
          (register gr-sym graph)
          (mk-qual gr-sym))
        (let [rt-sym (gensym "ohua-generated-runnable")]
          (register rt-sym (ohua.Runtime/prepare graph))
          (final-code-modifier `(do ~(gen-init-code) (.run ~(mk-qual rt-sym)))))))))


(defmacro ohua-require [& code] `(ohua.link/ohua-require ~@code))

(defmacro defsfn [name init-expr & body]
  `(defn ~(vary-meta name assoc :init-state `(quote ~init-expr)) ~@body))

(defmacro ohua
  "See `ohua-fn`."
  ([code] (ohua-fn code {}))
  ([code options] (ohua-fn code options)))


(defmacro <-ohua
  "See `ohua-fn`."
  ([code] (ohua-fn code #{:capture}))
  ([code options]
    (ohua-fn code
      (+option options :capture))))
