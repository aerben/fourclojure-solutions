(ns fourclojure-solutions.test.core
  (:use [fourclojure-solutions.core])
  (:use [clojure.test]))


(deftest interpositionTest
  (is (= (interposition 0 [1 2 3]) [1 0 2 0 3]
         ) "Interpose of 0 [1 2 3] should be [1 0 2 0 3]."))



(deftest dropnTest
  (is (= (dropN [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (dropN [:a :b :c :d :e :f] 2) [:a :c :e])))

(run-tests)