(ns ohua.zipper
    (:require
      [clojure.zip :as zip])
    (:import [clojure.lang IPersistentVector IPersistentMap IPersistentList ISeq]))

(defmulti tree-branch? class)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? IPersistentVector [v] true)
(defmethod tree-branch? IPersistentMap [m] true)
(defmethod tree-branch? IPersistentList [l] true)
(defmethod tree-branch? ISeq [s] true)
(prefer-method tree-branch? IPersistentList ISeq)

(defmulti tree-children class)
(defmethod tree-children IPersistentVector [v] v)
(defmethod tree-children IPersistentMap [m] (seq m))
(defmethod tree-children IPersistentList [l] l)
(defmethod tree-children ISeq [s] s)
(prefer-method tree-children IPersistentList ISeq)

(defmulti tree-make-node (fn [node children] (class node)))
(defmethod tree-make-node IPersistentVector [v children] (with-meta (vec children) (meta v)))
(defmethod tree-make-node IPersistentMap [m children] (with-meta (apply hash-map (apply concat children)) (meta m)))
(defmethod tree-make-node IPersistentList [node children] (with-meta children (meta node)))
(defmethod tree-make-node ISeq [node children] (with-meta (apply list children) (meta node)))
(prefer-method tree-make-node IPersistentList ISeq)

(defn tree-zipper [node]
      (zip/zipper tree-branch? tree-children tree-make-node node))


(defn walk-expr [node]
      ; TODO recursive function that handles (nested) expressions
      )

(defn walk-code [code]
      (let [node (tree-zipper code)]
           (walk-expr node)))