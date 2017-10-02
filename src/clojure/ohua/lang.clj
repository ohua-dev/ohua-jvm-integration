(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]))


(defmacro defalgo []
          ; TODO
          )


(defn ohua-fn [code option]
  (case code
    :import (do
              (println "Using the :import version of the ohua macro is deprecated")
              (apply ohua.link/ohua-require-fn (map (fn [ns_] [ns_ :refer :all]) option)))
    (let [_ (println code)
          graph (ohua.Compiler/compileAndSpliceEnv
                  ohua.link/clj-linker
                  code)]
      (ohua.Runtime/prepare graph))))


(defmacro ohua
  ([code] (ohua-fn code {}))
  ([code options] (ohua-fn code options)))
