(ns ohua.lang
  (:require
    [ohua.zipper :as oz])
  (:import 
    (ohua.types Binding
                FnName)
    (ohua.alang Expr
                Expr$Var
                Expr$Let
                Expr$Apply
                Expr$Lambda
                Assignment
                Assignment$Direct
                Assignment$Destructure
                ResolvedSymbol
                ResolvedSymbol$Local
                ResolvedSymbol$Sf
                ResolvedSymbol$Env)))


(defn ->bnd [sym]
  (Binding/mk (name sym)))

(defn ->fn-name [sym]
  (FnName. (namespace sym) (name sym)))

(defn handle-destructure [a]
  (cond 
    (seq? a) (Assignment$Destructure. (into-array (map ->bnd a)))
    (symbol? a) (Assignment$Direct. (->bnd a))
    :else (throw (Exception. "unexpected type of binding"))))

(defn mk-lams [args]
  (apply comp (map (fn [arg] (fn [e] (Expr$Lambda. (handle-destructure arg) e))) args)))

(defn resolve [sym]
  (if (namespace sym)
    (ResolvedSymbol$Sf. (->fn-name sym))
    (ResolvedSymbol$Local. (->bnd sym))))

(defn convert-expr [binding-gen env-exprs expr]
  (let [go #(convert-expr binding-gen env-exprs %)
        handle-statements 
        (fn [seq]
          ((apply comp (map (fn [e] (fn [e2] (Expr$Let. (Assignment$Direct. (binding-gen)) (go e) e2))) (butlast expr))) (go (last expr))))]
    (cond
      (seq? expr) 
        (case (first expr)
          let ((apply comp (map (fn [[bnd val]] (fn [e] (Expr$Let. (handle-destructure bnd) (go val) e))) (partition 2 (second expr)))) (handle-statements (drop 2 expr)))
          fn ((mk-lams (second expr)) (handle-statements (drop 2 expr)))
          (case (count expr)
            0 (throw (Exception. "Empty sequence"))
            1 (go (first expr))
            (Expr$Apply. (go (butlast expr)) (go (last expr)))))
      (symbol? expr) (Expr$Var. (resolve expr)))))

(defn clj->alang [form]
  (let [env-exprs (atom {})
        taken #{}
        binding-gen (let [possible-names (for [dig (cons "" (iterate inc 0)) chr (map (comp str char) (range 97 123))]
                                            (str chr dig))
                          gen (atom (remove taken possible-names))]
                      (fn []
                        (let [l @gen]
                          (if (compare-and-set! gen l (rest l))
                            (first l)
                            (recur)))))
        alang-exprs (convert-expr binding-gen env-exprs form)]
    [alang-exprs @env-exprs]))

(defmulti alang->str type)
(defmethod alang->str Expr$Var [v] (str "Var { value = " (alang->str (.value v)) " }"))
(defmethod alang->str Expr$Apply [a] (str "Apply { function = " (alang->str (.function a)) ", argument = " (alang->str (.argument a)) " }"))
(defmethod alang->str Expr$Lambda [l] (str "Lambda { assignment = " (alang->str (.assignment l)) ", body = " (alang->str (.body l)) " }"))
(defmethod alang->str Expr$Let [l] (str "Let { assignment = " (alang->str (.assignment l)) ", value = " (alang->str (.value l)) ", body = " (alang->str (.body l)) " }"))
(defmethod alang->str ResolvedSymbol$Local [l] (str "Local { value = " (.value (.binding l)) " }"))
(defmethod alang->str ResolvedSymbol$Env [e] (str "Env { id = " (.id e) " }"))
(defmethod alang->str ResolvedSymbol$Sf [s] (str "Sf { name = " (.name (.fnName s)) "/" (.namespace (.fnName s)) " }"))

(defmacro defalgo []
          ; TODO
          )


(defmacro ohua [code]
          (oz/walk-code code) )
