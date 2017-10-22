(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]
            [ohua.util :refer [macroexpand-all]]))


(defmacro algo 
  "Compile an algo. This produces alang as output.
   However the advantage here is that as this compiles the algo all sf-refs should be absolute
   in the result. Also all other algos should be inlined already. This means this approach
   (in contrast to the old one) respects the scoping of the linker."
  [args & code]
  (let [a (ohua.Compiler/compileAlgo
            ohua.link/clj-linker
            (conj 'fn ; perhaps this should be `algo`
              (conj args code)))
        a-name (gensym "algo")]
    (intern a-name a)
    a))


(defmacro defalgo 
  "I am rather happy with the new relationship between `algo` and `defalgo` since (as I wanted from the beginning)
   `(defalgo name [args] code)` is now literally the same as `(def name (algo [args] code))`."
  [name & code]
  `(def ~name (algo ~@code)))


(defn ohua-fn 
  "Compile the code, create the graph, prepare the runtime and save the executable with a global, generated name.
   Returns code that invokes the saved executable when evaluated."
  [code option]
  (if (= :import code)
    (do
      (println "Using the :import version of the ohua macro is deprecated")
      (apply ohua.link/ohua-require-fn (map (fn [ns_] [ns_ :refer :all]) option)))
    (let [graph (ohua.Compiler/compileAndSpliceEnv
                  ohua.link/clj-linker
                  (macroexpand-all code))
          mk-qual (fn [thing] (if (namespace thing) thing (symbol (str *ns*) (name thing))))]
      (if (= :test-compile option)
        (let [gr-sym (gensym "graph")] 
          (intern *ns* gr-sym graph)
          (mk-qual gr-sym))
        (let [rt-sym (gensym "ohua-generated-runnable")] 
          (intern *ns* rt-sym (ohua.Runtime/prepare graph))
          `(.run ~(mk-qual rt-sym)))))))


(defmacro ohua-require [& code] `(ohua.link/ohua-require ~@code))


(defmacro ohua
  "See `ohua-fn`."
  ([code] (ohua-fn code {}))
  ([code options] (ohua-fn code options)))
