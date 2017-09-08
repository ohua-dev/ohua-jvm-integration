;
; ohua : link.clj
;
; Copyright (c) 2016, 2017 Sebastian Ertel, Justus Adam. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE.TXT file.
;

(ns ohua.link
  (:require [clojure.string :as string]
            [ohua.util.loader :refer [load-from-classpath]])
  (:import (clojure.lang Symbol Var)))


(def ^:private ohua-linker-ref :__ohua-linker)


(deftype Linker [alias-registry imported-namespaces])
(defn- mk-linker [] (->Linker (atom {}) (atom {})))
(defn- init-linker [] (let [l (mk-linker)] (alter-meta! *ns* assoc ohua-linker-ref l) l))

; This map specifies how builtin symbols are resolved.
; Key will be resolved to value
; but both values and keys are allowed in the program
; values resolve to themselves
(def builtins {'smap    'com.ohua.lang/smap,
               'smap-io 'com.ohua.lang/smap-io,
               'seq     'com.ohua.lang/seq
               'loop    'loop
               'recur   'recur
               'if      'if
               'algo*   'com.ohua.lang/algo*
               'let     'clojure.core/let
               })


(def is-builtin? (partial contains? (set (concat (vals builtins) (keys builtins)))))


(def get-backend
  (let [backend-ref (atom nil)]
    (fn []
      (if-let [b @backend-ref]
        b
        (reset! backend-ref
                (let [_ (require 'ohua.backend/clojure)
                      bck (var-get (clojure.core/resolve 'ohua.backend.clojure/backend))]
                  (.initialize bck)
                  bck))))))


(defn ^Linker get-linker []
  (if-let [linker (ohua-linker-ref (meta *ns*))]
    linker
    (init-linker)))


(defn ->ns-string [ns-name]
  (cond
    (symbol? ns-name) (if-let [ns (namespace ns-name)] ns (name ns-name))
    (string? ns-name) ns-name
    :else (throw (new IllegalArgumentException (str "ns-name must be symbol or string, not " (if (nil? ns-name) "nil" (type ns-name)))))))

(defn get-ns [ns-name]
  (into #{} (.listNamespace (get-backend) (->ns-string ns-name))))

; HACK this is necessary due to shortcomings of clojures macroexpand
(def create-ns-if-missing
  (let [registry (atom #{})]
    (fn [ns]
      (when-not (@registry ns)
        (swap! registry conj ns)
        (create-ns ns)))))

(defn import-ns [ns-name]
  (let [ns-name (->ns-string ns-name)]
    (if-let [res (@(.-imported_namespaces (get-linker)) ns-name)]
      res
      (let [loaded (get-ns ns-name)]
        (create-ns-if-missing (symbol ns-name))
        (swap! (.-imported_namespaces (get-linker)) assoc ns-name loaded)
        loaded))))

(defn get-imported-ns [ns-name]
  (@(.-imported_namespaces (get-linker)) (->ns-string ns-name)))

(defn is-imported? [ns-name]
  (contains? @(.-imported_namespaces (get-linker)) (->ns-string ns-name)))

(defn is-aliased? [ref]
  (let [reg @(.-alias_registry (get-linker))]
    (contains? reg (cond
                     (symbol? ref) (name ref)
                     (string? ref) ref
                     :else (throw (new IllegalArgumentException (str "Ref must be symbol or string, not " (type ref))))))))

(defn register-alias-bare [key value]
  (swap! (.-alias_registry (get-linker)) assoc key value))

(defn ohua-alias [qual-ref alias-ref]
  (assert (symbol? qual-ref))
  (assert (or (nil? (namespace qual-ref)) (is-imported? (namespace qual-ref))))
  (if (is-aliased? alias-ref) (println "Already has registered ref for " (str alias-ref)))
  (register-alias-bare alias-ref qual-ref))

(defn ohua-unalias [sym]
  (if (and (symbol? sym) (not (nil? (namespace sym))))
    sym
    (let [name- (cond
                  (symbol? sym) (name sym)
                  (string? sym) sym
                  :else (throw (IllegalArgumentException. "Unexpected type for name.")))]
      (@(.-alias_registry (get-linker)) name-))))

(defn resolve-ns [ns-name]
  (let [ns-name (->ns-string ns-name)]
    (if-let [ns-name
             (if-let [unaliased (ohua-unalias ns-name)]
               (cond
                 (symbol? unaliased) (if (nil? (namespace unaliased)) (name unaliased))
                 (string? unaliased) unaliased
                 :else nil)
               ns-name)]
      (get-imported-ns ns-name))))

(defn resolve [name-str]
  (let [sym (symbol name-str)]
    (if-let [unaliased (if (nil? (namespace sym))
                        (ohua-unalias sym)
                        sym)]
      (if-let [ns (resolve-ns (namespace unaliased))]
        (if (ns (name unaliased))
          (FnName. (str unaliased)))))))

(defn is-defined? [name]
  (and (symbol? name)
       (not (nil? (resolve name)))))

(defn ohua-require-fn
  "Handles :as and :refer, lacks :reload, :verbose etc."
  [& args]
  (doseq [spec args]
    (cond
      (symbol? spec) (do
                       (assert (not (namespace spec)))
                       (import-ns spec))
      (coll? spec) (let [[ns-ref & rest] spec
                         imported-ns (do
                                       (assert (not (namespace ns-ref)))
                                       (assert (even? (count rest)))
                                       (import-ns ns-ref))]
                     (doseq [[flag data] (partition 2 rest)]
                       (case flag
                         :as (do
                               (assert (and (symbol? data)
                                            (not (namespace data))))
                               (ohua-alias ns-ref (name data)))
                         :refer (let [ns-str (name ns-ref)
                                      data (case data
                                             :all (map symbol imported-ns)
                                             data)]
                                  (doseq [sym data]
                                    (assert (symbol? sym))
                                    (ohua-alias (symbol ns-str (name sym)) (name sym))))
                         (throw (new IllegalArgumentException (str "Unrecognized flag " flag))))))
      :else (throw (new IllegalArgumentException (str "Spec must be symbol or sequence, not " (type spec)))))))

(defmacro ohua-require [& args] (apply ohua-require-fn args))

;;;
;;; The linker is a bit special because it is used during compilation as well as execution.
;;; During compilation the linker loads the necessary operators (from Java code) such that
;;; a piece of Ohua code can be compiled successfully. Function definitions however are
;;; defined in Clojure and therefore stored here. The Java linker code for functions is
;;; only used at runtime.
;;;

(defn list-links
  "List all references currently linked."
  []
  (concat
    (mapcat
      (fn [[alias ref]]
        (if (and (symbol? ref) (nil? (namespace ref)))
          (map symbol (repeat (->ns-string alias)) (get-imported-ns ref))
          [(if (symbol? alias) alias (symbol alias))]))
      (seq @(.-alias_registry (get-linker))))
    (mapcat
      (fn [[ns-name ns-dict]]
        (map symbol (repeat ns-name) ns-dict))
      (seq @(.-imported_namespaces (get-linker))))))


(defn- normalize-sfn-lookup-type [val]
  (condp = (type val)
    Var (normalize-sfn-lookup-type (var-get val))
    String val
    Class (name val)
    Symbol (name val)
    false))

(defn sfn? [val]
  (not (nil? (resolve val))))


(defn is-unresolved-sfn? [thing]
  (and (symbol? thing)
       (namespace thing)
       (not (clojure.core/resolve thing))
       (sfn? thing)))

(defn make-sfn [sym]
  (let [sym (resolve sym)]                                  ; TODO later unnecessary when sym-resolve is done
    (.asStatefulFunction (get-backend) (namespace sym) (name sym))))


(reify ohua.Linker
  (resolve [_ n]
    (resolve n)))
