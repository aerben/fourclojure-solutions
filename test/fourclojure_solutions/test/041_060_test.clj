(ns fourclojure-solutions.test.041_060_test
  (:use [fourclojure-solutions.041_060])
  (:use [clojure.test])
  (:use [clojure.set]))

(deftest drop-nth-41-test
  (is (= (drop-nth-41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-nth-41 [:a :b :c :d :e :f] 2) [:a :c :e]))
  (is (= (drop-nth-41 [1 2 3 4 5 6] 4) [1 2 3 5 6])))

(deftest factorial-42-test
  (is (= (factorial-42 1) 1))
  (is (= (factorial-42 3) 6))
  (is (= (factorial-42 5) 120))
  (is (= (factorial-42 8) 40320)))

(deftest reverse-interleave-43-test
  (is (= (reverse-interleave-43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave-43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave-43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest rotate-44-test
  (is (= (rotate-44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-44 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-44 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-44 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-44 -4 '(:a :b :c)) '(:c :a :b))))

(deftest flip-46-test
  (is (= 3 ((flip-46 nth) 2 [1 2 3 4 5])))
  (is (= true ((flip-46 >) 7 8)))
  (is (= 4 ((flip-46 quot) 2 8)))
  (is (= [1 2 3] ((flip-46 take) [1 2 3 4 5] 3))))

(deftest split-at-49-test
  (is (= (split-at-49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-at-49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-at-49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest split-by-type-50-test
  (is (= (set (split-by-type-50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type-50 [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type-50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest longest-increasing-subseq-53-test
  (is (= (longest-increasing-subseq-53 [1 0 1 2 3 0 4 5]) [0 1 2 3]))
  (is (= (longest-increasing-subseq-53 [5 6 1 3 2 7]) [5 6]))
  (is (= (longest-increasing-subseq-53 [2 3 3 4 5]) [3 4 5]))
  (is (= (longest-increasing-subseq-53 [7 6 5 4]) [])))

(deftest partition-54-test
  (is (= (partition-54 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (partition-54 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))) )
  (is (= (partition-54 3 (range 8)) '((0 1 2) (3 4 5)))))

(deftest frequencies-55-test
  (is (= (frequencies-55 [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (frequencies-55 [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (frequencies-55 '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(deftest distinct-56-test
  (is (= (distinct-56 [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (distinct-56 [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (distinct-56 '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (distinct-56 (range 50)) (range 50))))

(deftest composition-58-test
  (is (= [3 2 1] ((composition-58 rest reverse) [1 2 3 4])))
  (is (= 5 ((composition-58 (partial + 3) second) [1 2 3 4])))
  (is (= true ((composition-58 zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((composition-58 #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(deftest juxtaposition-59-test
  (is (= [21 6 1] ((juxtaposition-59 + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxtaposition-59 #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxtaposition-59 :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(run-tests)