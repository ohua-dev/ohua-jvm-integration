(ns ohua.lang
    (:require
      [ohua.zipper :as oz]))


(defmacro defalgo
          ; TODO
          )


(defmacro ohua [code]
          (oz/walk-code code) )