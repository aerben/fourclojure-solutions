(ns fourclojure-solutions.test.081_100_test
  (:use [fourclojure-solutions.081_100])
  (:use [clojure.test])
  (:use [clojure.set]))

(deftest intersection-81-test
  (= (intersection-81 #{0 1 2 3} #{2 3 4 5}) #{2 3})
  (= (intersection-81 #{0 1 2} #{3 4 5}) #{})
  (= (intersection-81 #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))

(deftest word-chains-82-test
  (is (= true (word-chains-82 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
  (is (= false (word-chains-82 #{"cot" "hot" "bat" "fat"})))
  (is (= false (word-chains-82 #{"to" "top" "stop" "tops" "toss"})))
  (is (= true (word-chains-82 #{"spout" "do" "pot" "pout" "spot" "dot"})))
  (is (= true (word-chains-82 #{"share" "hares" "shares" "hare" "are"})))
  (is (= false (word-chains-82 #{"share" "hares" "hare" "are"}))))

(deftest half-true-83-test
  (is (= false (half-true-83 false false)))
  (is (= false (half-true-83 false false)))
  (is (= true (half-true-83 true false)))
  (is (= false (half-true-83 true)))
  (is (= true (half-true-83 false true false)))
  (is (= false (half-true-83 true true true)))
  (is (= true (half-true-83 true true true false))))

(run-tests)