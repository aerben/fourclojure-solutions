(ns fourclojure-solutions.test.fourclojureTest
  (:use [fourclojure-solutions.fourclojure])
  (:use [clojure.test]))

(deftest truthy83Test
  (is (= false (truthy_83 false false)))
  (is (= true (truthy_83 true false)))
  (is (= false (truthy_83 true true true))))

(deftest symmetricDiff88Test
  (is (= (symmetricDiff_88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest dotproduct143Test
  (is (= 256 (dotproduct_143 [2 5 6] [100 10 1]))))

(deftest comparison166Test
  (is (= :gt (comparison_166 < 5 1)))
  (is (= :eq (comparison_166 (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparison_166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparison_166 > 0 2))))

(run-tests)