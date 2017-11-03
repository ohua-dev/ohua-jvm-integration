;
; ohua : linker_test_2.clj
;
; Copyright (c) Sebastian Ertel, Justus Adam 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE.TXT file.
;

(ns ohua.linker-test-2
  (:require [ohua.link :refer :all]
            [clojure.test :refer [deftest is]]
            [ohua.linker-test]))
(ohua-require [ohua.lang.tests :refer [write] :as tests])


(deftest linker-spill-test
  (is (not (is-imported? 'ohua.tests)))
  (is (nil? (resolve 'ohua.test/write))))

(deftest alias-spill-test
  (is (not (is-aliased? 'tests)))
  (is (nil? (resolve 'tests/accept)))
  (is (not (is-aliased? 'write)))
  (is (nil? (resolve 'write))))
