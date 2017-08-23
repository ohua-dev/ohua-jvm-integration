(ns ohua.lang
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
                ResolvedSymbol$Env))
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn ->bnd [sym]
  (Binding/mk (name sym)))

(defn ->fn-name [sym]
  (FnName. (namespace sym) (name sym)))

(defn ->algo-bnd [sym]
  (Binding/mk (str (namespace sym) "/" (name sym))))

(defn handle-destructure [a]
  (cond 
    (seq? a) (Assignment$Destructure. (into-array (map ->bnd a)))
    (symbol? a) (Assignment$Direct. (->bnd a))
    :else (throw (Exception. "unexpected type of binding"))))

(defn mk-lams [args]
  (apply comp (map (fn [arg] (fn [e] (Expr$Lambda. (handle-destructure arg) e))) args)))

(defn resolve-type [sym]
  [nil (symbol "test" (name sym))])


(defn flatten-assign [thing]
  (if (symbol? thing)
    [thing]
    thing))

(defn convert-expr [binding-gen env-exprs _scope expr]
  (let [scope (atom _scope)
        go #(convert-expr binding-gen env-exprs @scope %)
        add-to-scope (fn [bnds] (swap! scope set/union (set bnds)))
        register-env-expr (fn [e]
                            (let [d @env-exprs]
                              (if (compare-and-set! env-exprs d (conj d e))
                                (count d)
                                (recur e))))
        handle-statements 
        (fn [seq]
          ((apply comp (map (fn [e] (fn [e2] (Expr$Let. (Assignment$Direct. (binding-gen)) (go e) e2))) (butlast expr))) (go (last expr))))]
    (cond
      (seq? expr) 
        (case (first expr)
          let ((apply comp (map (fn [[bnd val]] (fn [e] (add-to-scope (flatten-assign bnd)) (Expr$Let. (handle-destructure bnd) (go val) e))) (partition 2 (second expr)))) (handle-statements (drop 2 expr)))
          fn (do (add-to-scope (second expr)) ((mk-lams (second expr)) (handle-statements (drop 2 expr))))
          (case (count expr)
            0 (throw (Exception. "Empty sequence"))
            1 (go (first expr))
            (Expr$Apply. (go (butlast expr)) (go (last expr)))))
      (symbol? expr) (Expr$Var. (if (@scope expr) 
                                  (ResolvedSymbol$Local. (->bnd expr)) 
                                  (let [[type full-name] (resolve-type expr)]
                                    (case type
                                      :algo (ResolvedSymbol$Local. (->algo-bnd full-name))
                                      :sf (ResolvedSymbol$Sf. (->fn-name full-name))
                                      (ResolvedSymbol$Env. (register-env-expr expr))))))
      :else (Expr$Var. (ResolvedSymbol$Env. (register-env-expr expr))))))

(defn clj->alang [form]
  (let [env-exprs (atom [])
        taken #{}
        binding-gen (let [possible-names (for [dig (cons "" (iterate inc 0)) chr (map (comp str char) (range 97 123))]
                                            (str chr dig))
                          gen (atom (remove taken possible-names))]
                      (fn []
                        (let [l @gen]
                          (if (compare-and-set! gen l (rest l))
                            (Binding/mk (first l))
                            (recur)))))
        alang-exprs (convert-expr binding-gen env-exprs #{} form)]
    [alang-exprs @env-exprs]))

(defmulti alang->str type)
(defmethod alang->str Expr$Var [v] (str "Var { value = " (alang->str (.value v)) " }"))
(defmethod alang->str Expr$Apply [a] (str "Apply { function = " (alang->str (.function a)) ", argument = " (alang->str (.argument a)) " }"))
(defmethod alang->str Expr$Lambda [l] (str "Lambda { assignment = " (alang->str (.assignment l)) ", body = " (alang->str (.body l)) " }"))
(defmethod alang->str Expr$Let [l] (str "Let { assignment = " (alang->str (.assignment l)) ", value = " (alang->str (.value l)) ", body = " (alang->str (.body l)) " }"))
(defmethod alang->str ResolvedSymbol$Local [l] (str "Local { value = " (.value (.binding l)) " }"))
(defmethod alang->str ResolvedSymbol$Env [e] (str "Env { id = " (.id e) " }"))
(defmethod alang->str ResolvedSymbol$Sf [s] (str "Sf { name = " (.name (.fnName s)) "/" (.namespace (.fnName s)) " }"))
(defmethod alang->str Assignment$Direct [a] (str "Direct " (alang->str (.binding a))))
(defmethod alang->str Assignment$Destructure [d] (str "Destructure [" (str/join "," (map alang->str (.bindings d))) "]"))
(defmethod alang->str Binding [b] (str (.value b)))


(defn eval-and-print [expr]
  (let [[alang envs] (clj->alang expr)]
    (println (alang->str alang))
    (println envs)))


(defmacro defalgo []
          ; TODO
          )


(defmacro ohua [code] )
