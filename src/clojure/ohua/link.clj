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
  (:import (clojure.lang Symbol Var)
           (ohua StatefulFunctionProvider)
           (ohua.loader MultiDispatchSFProvider JavaProviderFromAnnotatedMethod)))


(def ^:private ohua-linker-ref :__ohua-linker)


(deftype Linker [alias-registry imported-namespaces])
(defn- mk-linker [] (->Linker (atom {}) (atom {})))
(defn- init-linker [] (let [l (mk-linker)] (alter-meta! *ns* assoc ohua-linker-ref l) l))


(defn ^Linker get-linker []
  (if-let [linker (ohua-linker-ref (meta *ns*))]
    linker
    (init-linker)))


(defn ->ns-string [ns-name]
  (cond
    (symbol? ns-name) (if-let [ns (namespace ns-name)] ns (name ns-name))
    (string? ns-name) ns-name
    :else (throw (new IllegalArgumentException (str "ns-name must be symbol or string, not " (if (nil? ns-name) "nil" (type ns-name)))))))

(defn import-ns [ns-name]
  (swap! (.-imported_namespaces (get-linker)) conj (->ns-string ns-name)))

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


(def backend (MultiDispatchSFProvider/combine (into-array StatefulFunctionProvider [
  (JavaProviderFromAnnotatedMethod.)
  ; (reify StatefulFunctionProvider
  ;   (provide [this ns-ref sf-ref]
  ;     ; create a stateful function object
  ;     (clojure.core/resolve (symbol ns-ref sf-ref)))
  ;   (exists [this ns-ref sf-ref]
  ;     (not (nil? (clojure.core/resolve (symbol ns-ref sf-ref))))))
  ])))

(defn resolve [name-str]
  (let [sym (symbol name-str)]
    (if-let [unaliased (if (nil? (namespace sym))
                        (ohua-unalias sym)
                        sym)]
      (and
        (contains? @(.-imported_namespaces (get-linker)) (namespace unaliased))
        (.exists backend (namespace unaliased) (name unaliased)))
      false)))

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


(def clj-linker
  (reify ohua.Linker
    (resolve [_ n]
      (resolve n))))
