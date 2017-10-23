(ns ohua.apply-test
  (:require [ohua.lang :refer [ohua]]
            [clojure.test :refer [deftest is]]
            ;[ohua.logging :as l]
            ))


(ohua :import [ohua.lang.tests])


(deftest simple-apply-test
  (is
    (=
      120
      (ohua (apply clojure.core/+ (id 20) 100))))
  ; disabled for now, as they use embedded clojure functions which are currently not supported
  ; (is
  ;   (=
  ;     120
  ;     (ohua (id (apply ohua.lang.tests/add (int 20) (int 100))))))
  ; (is
  ;   (=
  ;     120
  ;     (ohua (apply ohua.lang.tests/add (id (int 20)) (int 100)))))
      )
