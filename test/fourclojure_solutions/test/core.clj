(ns fourclojure-solutions.test.core
  (:use [fourclojure-solutions.core])
  (:use [clojure.test]))


(deftest interposition40Test
  (is (= (interposition_40 0 [1 2 3]) [1 0 2 0 3]
         ) "Interpose of 0 [1 2 3] should be [1 0 2 0 3]."))

(deftest dropn41Test
  (is (= (dropN_41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (dropN_41 [:a :b :c :d :e :f] 2) [:a :c :e])))

(deftest split49Test
  (is (= (split49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(run-tests)