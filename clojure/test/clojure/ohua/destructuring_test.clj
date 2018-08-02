(ns ohua.destructuring-test
  (:require [ohua.lang :refer [<-ohua ohua ohua-require]]
            [clojure.test :refer [is deftest]]))

(ohua-require [ohua.tests :refer [objectArrayId]])

(def third-level-arr (into-array [:q :b]))

(def third-elem {:q 8})

(def second-level-arr (into-array Object [third-level-arr third-elem]))

(def second-elem #{:b})

(def top-level-arr (into-array Object [second-level-arr second-elem]))


(deftest flat-destructuring
  (is 
    (<-ohua 
      (let [[f s] (id top-level-arr)]
        (and
          (identical? f second-level-arr)
          (identical? s second-elem)))))
  (is
    (<-ohua 
      ((algo [[f s]]
        (and
          (identical? f second-level-arr)
          (identical? s second-elem))) (id top-level-arr))))

  ; the following are the same test cases but using the stateful function from 
  ; ohua.tests.DebugDestructuringTest which is `id` but with `Object[]` return type
  ; This is to show that is *does* work as long as the return type is `Object[]`
  (is 
    (<-ohua 
      (let [[f s] (objectArrayId top-level-arr)]
        (and
          (identical? f second-level-arr)
          (identical? s second-elem)))))
  (is
    (<-ohua 
      ((algo [[f s]]
        (and
          (identical? f second-level-arr)
          (identical? s second-elem))) (objectArrayId top-level-arr)))))

; (deftest one-level-nested-destructuring
;   (is 
;     (<-ohua
;       (let [[[a b] _] (id top-level-arr)]
;         (and 
;           (identical? a third-level-arr)
;           (identical? b third-elem)))))
;   (is 
;     (<-ohua
;       ((algo [[[a b] _]]
;         (and 
;           (identical? a third-level-arr)
;           (identical? b third-elem)))
;         (id top-level-arr)))))


; (deftest two-level-nested-destructuring
;   (is 
;     (<-ohua
;       (let [[[[a b] _] _] (id top-level-arr)]
;         (and 
;           (identical? a :q)
;           (identical? b :b)))))
;   (is 
;     (<-ohua
;       ((algo [[[[a b] _] _]]
;         (and 
;           (identical? a :q)
;           (identical? b :b)))
;         (id top-level-arr)))))


