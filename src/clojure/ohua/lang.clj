(ns ohua.lang
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]))


(defmacro defalgo []
          ; TODO
          )


(defmacro ohua [code options]
  (let [graph (ohua.Compiler/compileAndSpliceEnv
                ohua.link/clj-linker
                code)]
    (ohua.Runtime/prepare graph)))
