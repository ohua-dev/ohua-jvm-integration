;
; ohua : linker_test.clj
;
; Copyright (c) Sebastian Ertel, Justus Adam 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE.TXT file.
;

(ns ohua.linker-test
  (:require [ohua.link :refer :all]
            [clojure.test :refer [deftest is use-fixtures]]))


(defn fixture [f]
  (ohua-require-fn '[ohua.tests :refer [write] :as tests])
  (f))

(use-fixtures :once fixture)


(deftest import-test
  (is (is-imported? 'ohua.tests))
  (is (not (nil? (resolve 'ohua.tests/write))))
  (is (not (nil? (resolve 'ohua.tests/parse)))))

(deftest refer-test
  (is (is-aliased? 'write))
  (is (not (nil? (resolve 'write))))
  (is (=
        (resolve 'write)
        (resolve 'ohua.tests/write))))

(deftest as-test
  (is (is-aliased? 'tests))
  (is (not (nil? (resolve 'tests/accept))))
  (is (=
        (resolve 'tests/accept)
        (resolve 'ohua.tests/accept)))
  (is (not (nil? (resolve 'tests/write)))))
