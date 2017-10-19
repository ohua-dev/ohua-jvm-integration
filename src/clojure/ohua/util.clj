(ns ohua.util
  (:require [clojure.walk :as w]))



(defn macroexpand-all 
  "A custom version of `clojure.walk/macroexpand-all` which does not expand `let` bindings."
  [form]
  (w/prewalk #(if (and (seq? %) (not (= 'let (first %)))) (macroexpand %) %) form))
