(ns fourclojure-solutions.test.fourclojureTest
  (:use [fourclojure-solutions.fourclojure])
  (:use [clojure.test]))


(deftest interposition40Test
  (is (= (interposition_40 0 [1 2 3]) [1 0 2 0 3]
         ) "Interpose of 0 [1 2 3] should be [1 0 2 0 3]."))

(deftest dropn41Test
  (is (= (dropN_41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (dropN_41 [:a :b :c :d :e :f] 2) [:a :c :e])))

(deftest split49Test
  (is (= (split_49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split_49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split_49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest truthy83Test
  (is (= false (truthy_83 false false)))
  (is (= true (truthy_83 true false)))
  (is (= false (truthy_83 true true true))))

(deftest repeater33Test
  (is (= (repeater_33 [:a :b] 4) '(:a :a :a :a :b :b :b :b))))

(comment
  (deftest splitType50Test
    (is (= (set (splitType50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (splitType50 [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (splitType50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))
  )

(run-tests)