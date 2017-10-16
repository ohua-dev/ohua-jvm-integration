(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]
            [clojure.walk :refer [macroexpand-all]]))


(defmacro defalgo []
          ; TODO
          )


(defn ohua-fn [code option]
  (if (= :import code)
    (do
      (println "Using the :import version of the ohua macro is deprecated")
      (apply ohua.link/ohua-require-fn (map (fn [ns_] [ns_ :refer :all]) option)))
    (let [_ (println (macroexpand-all code))
          graph (ohua.Compiler/compileAndSpliceEnv
                  ohua.link/clj-linker
                  (macroexpand-all code))
          prepared-rt-sym (gensym "graph")]
      (intern *ns* prepared-rt-sym (ohua.Runtime/prepare graph))
      `(.run ~prepared-rt-sym))))


(defmacro ohua
  ([code] (ohua-fn code {}))
  ([code options] (ohua-fn code options)))
