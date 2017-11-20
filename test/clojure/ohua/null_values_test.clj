(ns ohua.null-values-test
  (:require [clojure.test :refer :all :as test]
            [ohua.lang :refer :all]
            [ohua.testutils :refer [mk-cond]]))

(ohua-require [ohua.tests :refer [add subtract]])

(def _100 (int 100))

; Issue #10
(deftest null-value-propagation
  "Making sure we do not drop null values. (In which case this test would deadlock.)"
  (test/is
    (=
      0
      (<-ohua
        (let [one (id _100)
              c (id nil)]
          (if (mk-cond c)
            (add one _100)
            (subtract one _100))))
      )
    )
  )
