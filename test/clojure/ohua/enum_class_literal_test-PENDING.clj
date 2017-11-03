(ns ohua.enum-class-literal-test
  (:require [ohua.lang :refer [<-ohua ohua]]
            [clojure.test :refer [deftest is]])
  (:import [ohua.tests EnumTestUtil EnumTestUtil$AnEnum ClassTestUtil ClassTestUtil$MyClass]))

(ohua :import [ohua.tests])

; Run this after closing ohua-jvm-runtime#3

(def zero (int 0))

(deftest enum-in-first
  (is (<-ohua (isValue1 zero EnumTestUtil$AnEnum/Value1))))

; (deftest enum-in
;   (is (<-ohua (isValue1 (int 1) nil EnumTestUtil$AnEnum/Value1 nil))))


; (deftest enum-in-1-with-sfn
;   (is (<-ohua (isValue1 (int 1) (add (int 1) (int 2)) EnumTestUtil$AnEnum/Value1 (add (int 40) (int 60))))))

; (deftest enum-in-0-with-sfn
;   (is (<-ohua (isValue1 (int 0) EnumTestUtil$AnEnum/Value1 (add (int 1) (int 2)) (add (int 40) (int 60))))))

; (deftest enum-in-2-with-sfn
;   (is (<-ohua (isValue1 (int 2) (add (int 1) (int 2)) (add (int 40) (int 60)) EnumTestUtil$AnEnum/Value1))))


; (deftest class-in-first
;   (is (<-ohua (isClass (int 0) ClassTestUtil$MyClass))))

; (deftest class-in
;   (is (<-ohua (isClass (int 1) nil ClassTestUtil$MyClass nil))))

; (deftest class-in-with-sfn
;   (is (<-ohua (isClass (int 1) (add (int 1) (int 2)) ClassTestUtil$MyClass (add (int 40) (int 60))))))

; (deftest nil-literal
;   (is true (<-ohua (nil? nil))))
