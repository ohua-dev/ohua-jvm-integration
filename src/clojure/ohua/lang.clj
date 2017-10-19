(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]
            [clojure.walk :refer [macroexpand-all]]))


(defmacro algo [args & code]
  (let [a (ohua.Compiler/compileAlgo
            ohua.link/clj-linker
            (conj 'fn ; perhaps this should be `algo`
              (conj args code)))
        a-name (gensym "algo")]
    (intern a-name a)
    a))


(defmacro defalgo [name & code]
  `(def ~name (algo ~@code)))


(defn ohua-fn [code option]
  (if (= :import code)
    (do
      (println "Using the :import version of the ohua macro is deprecated")
      (apply ohua.link/ohua-require-fn (map (fn [ns_] [ns_ :refer :all]) option)))
    (let [graph (ohua.Compiler/compileAndSpliceEnv
                  ohua.link/clj-linker
                  code) ; macroexpand this at some point to get support for macros
          prepared-rt-sym (gensym "graph")]
      (intern *ns* prepared-rt-sym (ohua.Runtime/prepare graph))
      `(.run ~prepared-rt-sym))))


(defmacro ohua
  ([code] (ohua-fn code {}))
  ([code options] (ohua-fn code options)))
