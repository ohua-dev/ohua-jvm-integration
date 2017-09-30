;;;
;;; Copyright (c) Sebastian Ertel 2014. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(ns ohua.logging
  (:require [clojure.pprint]))

; this flag should be used to shut down all logging in the system
(def logging-allowed (atom true))
(defmacro allow-logging [] (swap! logging-allowed (fn [_] true)))

; use this flag to shut down individual logging
(def logging-enabled (atom false))
(defn enable-logging [] (if @logging-allowed (swap! logging-enabled (fn [_] true)) false))
(defn disable-logging [] (swap! logging-enabled (fn [_] false)))

(defmacro disallow-logging []
  (disable-logging )
  (swap! logging-allowed (fn [_] false)))

; if we want to enable logging for compilation then this needs to be a macro!
(defmacro enable-compilation-logging [] (enable-logging))

(defn logging-enabled? [] @logging-enabled)

(defn printline [& msgs]
  (if @logging-enabled (apply println msgs)))

(defn pprint [s]
  (if @logging-enabled (clojure.pprint/pprint s)))

(defn write [& s]
  (if @logging-enabled (apply clojure.pprint/write s)))

(defn write-code [s]
  (if @logging-enabled
    (let [*code-dispatch* clojure.pprint/code-dispatch]
      (clojure.pprint/pprint s))))

(defn print-table [s]
  (if @logging-enabled (clojure.pprint/print-table s)))


(defmacro print-meta [c]
  ; https://groups.google.com/forum/#!topic/clojure/5LRmPXutah8
  `(let [orig-dispatch# clojure.pprint/*print-pprint-dispatch*]
     (clojure.pprint/with-pprint-dispatch
       (fn [o#]
         (when (meta o#)
           (print "^")
           (orig-dispatch# (meta o#))
           (clojure.pprint/pprint-newline :fill))
         (orig-dispatch# o#))
       (clojure.pprint/pprint ~c)))
  )
