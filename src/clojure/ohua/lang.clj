(ns ohua.lang
    (:require
      [ohua.zipper :as oz]))


(defn ->bnd [sym]
  (Binding. (str sym)))

(defn ->fn-name [sym]
  (FnName. (.namespace sym) (.name sym)))

(defn handle-destructure [a]
  (cond 
    (seq? a) (Assignment$Destructure. (into-array (map ->bnd a)))
    (symbol? a) (Assignment$Direct. (->bnd a))
    :else (throw (Exception. "unexpected type of binding"))))

(defn mk-lams [args]
  (apply comp (map (fn [arg] (fn [e] (Expr$Lambda. (handle-destructure arg) e))) args)))

(defn clj->alang [form]
  (let [env-exprs (atom {})
        binding-gen (fn [])
        go  (let [handle-statements 
                  (fn [binding-gen seq]
                    ((apply comp (map (fn [e] (fn [e2] (Expr$Let. (Assignment$Direct (binding-gen)) (go e) e2))) (butlast expr))) (go (last expr))))]
              (fn [expr]
                (cond
                  (seq? expr) 
                    (case (first expr)
                      let ((apply comp (map (fn [[bnd val]] (fn [e] (Expr$Let. (handle-destructure bnd) (go val) e))) (partition 2 (second expr)))) (handle-statements (drop 2 expr)))
                      fn ((mk-lams (second expr)) (handle-statements binding-gen (drop 2 expr)))
                      (case (length expr)
                        0 (throw (Exception. "Empty sequence"))
                        1 (go (first expr))
                        (Expr$Apply (go (butlast expr)) (go (last expr)))))
                  (symbol? expr) (Expr$Var. 
                                    (if (resolvable? expr) 
                                      (ResolvedSymbol$Sf. (->fn-name expr))
                                      (ResolvedSymbol$Local. (->bnd expr))))
                ))
        alang-exprs (go form)])
    [alang-exprs @env-exprs])


(defmacro defalgo
          ; TODO
          )


(defmacro ohua [code]
          (oz/walk-code code) )
