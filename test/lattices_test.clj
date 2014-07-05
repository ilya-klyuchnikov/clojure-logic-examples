(ns lattices-test
  (:refer-clojure :exclude [== record?])
  (:use [clojure.core.logic :exclude [is record?] :as l]
        [clojure.test :refer :all]
        [lattices :refer :all]))

(defn solvej [x y z]
  (cond
    (and (= x :?) (= y :?) (= z :?)) (set (run* [q] (l-join q q q)))
    (and (= x :?) (= y :?)) (set (run* [q] (l-join q q z)))
    (and (= x :?) (= z :?)) (set (run* [q] (l-join q y q)))
    (and (= y :?) (= z :?)) (set (run* [q] (l-join x q q)))
    (= x :?) (set (run* [q] (l-join q y z)))
    (= y :?) (set (run* [q] (l-join x q z)))
    (= z :?) (set (run* [q] (l-join x y q)))))

(defn solvem [x y z]
  (cond
    (and (= x :?) (= y :?) (= z :?)) (set (run* [q] (l-meet q q q)))
    (and (= x :?) (= y :?)) (set (run* [q] (l-meet q q z)))
    (and (= x :?) (= z :?)) (set (run* [q] (l-meet q y q)))
    (and (= y :?) (= z :?)) (set (run* [q] (l-meet x q q)))
    (= x :?) (set (run* [q] (l-meet q y z)))
    (= y :?) (set (run* [q] (l-meet x q z)))
    (= z :?) (set (run* [q] (l-meet x y q)))))

(deftest lattice-elements
  (is (= (run* [q] (l-el q)) [:bot :null :not-null :top])))

(deftest test-join
  (is (= (solvej :top :top :?) #{:top}))
  (is (= (solvej :null :? :top) #{:top :not-null}))
  (is (= (solvej :null :? :null) #{:bot :null}))
  (is (= (solvej :null :? :?) #{:top :null}))
  (is (= (solvej :? :? :?) #{:bot :null :not-null :top})))

(deftest test-meet
  (is (= (solvem :top :top :?) #{:top}))
  (is (= (solvem :null :? :top) #{}))
  (is (= (solvem :null :? :null) #{:null :top}))
  (is (= (solvem :null :? :?) #{:bot :null}))
  (is (= (solvem :? :? :?) #{:bot :null :not-null :top})))
