(ns fourclojure-solutions.test.021_040_test
  (:use [fourclojure-solutions.021_040])
  (:use [clojure.test])
  (:use [clojure.set]))

(deftest nth-21-test
  (is (= (nth-21 '(4 5 6 7) 2) 6))
  (is (= (nth-21 [:a :b :c] 0) :a))
  (is (= (nth-21 [1 2 3 4] 1) 2))
  (is (= (nth-21 '([1 2] [3 4] [5 6]) 2) [5 6])))

(deftest count-22-test
  (is (= (count-22 '(1 2 3 3 1)) 5))
  (is (= (count-22 "Hello World") 11))
  (is (= (count-22 [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-22 '(13)) 1))
  (is (= (count-22 '(:a :b :c)) 3)))

(deftest reverse-23-test
  (is (= (reverse-23 [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-23 (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-23 [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

(deftest sum-24-test
  (is (= (sum-24 [1 2 3]) 6))
  (is (= (sum-24 (list 0 -2 5 5)) 8))
  (is (= (sum-24 #{4 2 1}) 7))
  (is (= (sum-24 '(0 0 -1)) -1))
  (is (= (sum-24 '(1 10 3)) 14)))

(deftest filter-odd-25-test
  (is (= (filter-odd-25 #{1 2 3 4 5}) '(1 3 5)))
  (is (= (filter-odd-25 [4 2 1 6]) '(1)))
  (is (= (filter-odd-25 [2 2 4 6]) '()))
  (is (= (filter-odd-25 [1 1 1 3]) '(1 1 1 3))))

(deftest fib-n-26-test
  (is (= (fib-n-26 3) '(1 1 2)))
  (is (= (fib-n-26 6) '(1 1 2 3 5 8)))
  (is (= (fib-n-26 8) '(1 1 2 3 5 8 13 21))))

(deftest palindrome-27-test
  (is (false? (palindrome-27 '(1 2 3 4 5))))
  (is (true? (palindrome-27 "racecar")))
  (is (true? (palindrome-27 [:foo :bar :foo])))
  (is (true? (palindrome-27 '(1 1 3 3 1 1))))
  (is (false? (palindrome-27 '(:a :b :c)))))

(deftest flatten-28-test
  (is (= (flatten-28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-28 ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-28 '((((:a))))) '(:a))))

(deftest caps-29-test
  (is (= (caps-29 "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (caps-29 "nothing")))
  (is (= (caps-29 "$#A(*&987Zf") "AZ")))

(deftest compress-30-test
  (is (= (apply str (compress-30 "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(deftest pack-31-test
  (is (= (pack-31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

(deftest duplicate-32-test
  (is (= (duplicate-32 [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (duplicate-32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(deftest replicate-33-test
  (is (= (replicate-33 [1 2 3] 2) '(1 1 2 2 3 3)))
  (is (= (replicate-33 [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (is (= (replicate-33 [4 5 6] 1) '(4 5 6)))
  (is (= (replicate-33 [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (replicate-33 [44 33] 2) [44 44 33 33])))

(run-tests)