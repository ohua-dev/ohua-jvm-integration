(ns ohua.lang
  (:import 
    (ohua Compiler
          Runtime))
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ohua.link]))


(defmacro defalgo []
          ; TODO
          )


(defmacro ohua [code] 
  (let [graph (Compiler/compileAndSpliceEnv 
                ohua.link/clj-linker
                code)]
    (Runtime/prepare graph)))
