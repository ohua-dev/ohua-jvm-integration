(ns ohua.util
  (:require [clojure.walk :as w]))



(defn macroexpand-all 
  "A custom version of `clojure.walk/macroexpand-all` which does not expand `let` bindings."
  [form]
  (w/prewalk #(if (and (seq? %) (not (= 'let (first %)))) (macroexpand %) %) form))


(defn report-option-type [option]
  (throw (Exception. (str "Unexpected type of option. Exprected set, map or keyword, got " (type option)))))


(defn +option [map option]
  
  (cond 
    (set? map) (conj map option)
    (map? map) (assoc map option true)
    (keyword? map) (conj #{map} option)
    :else (report-option-type map)))
