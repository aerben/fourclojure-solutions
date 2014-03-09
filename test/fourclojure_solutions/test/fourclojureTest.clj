(ns fourclojure-solutions.test.fourclojureTest
  (:use [fourclojure-solutions.fourclojure])
  (:use [clojure.test]))

(deftest flatten28Test
  (is (= (flat_28 ["a" ["b"] "c"]) '("a" "b" "c"))))

(comment
  (deftest compress30Test

    (is (= (apply str (compress_30 "Leeeeeerrroyyy")) "Leroy"))
    (is (= (compress_30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
    (is (= (compress_30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

(deftest repeater33Test
  (is (= (repeater_33 [:a :b] 4) '(:a :a :a :a :b :b :b :b))))

(deftest interposition40Test
  (is (= (interposition_40 0 [1 2 3]) [1 0 2 0 3]
         ) "Interpose of 0 [1 2 3] should be [1 0 2 0 3]."))

(deftest dropn41Test
  (is (= (dropN_41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (dropN_41 [:a :b :c :d :e :f] 2) [:a :c :e])))

(deftest factorial42Test
  (is (= (factorial_42 8) 40320)))

(deftest split49Test
  (is (= (split_49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split_49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split_49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))


(comment
  (deftest splitType50Test
    (is (= (set (splitType50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (splitType50 [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (splitType50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))
  )

(deftest truthy83Test
  (is (= false (truthy_83 false false)))
  (is (= true (truthy_83 true false)))
  (is (= false (truthy_83 true true true))))

(deftest symmetricDiff88Test
  (is (= (symmetricDiff_88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest comparison166Test
  (is (= :gt (comparison_166 < 5 1)))
  (is (= :eq (comparison_166 (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparison_166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparison_166 > 0 2))))



(run-tests)