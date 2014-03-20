(ns fourclojure-solutions.test.001_020_test
  (:use [fourclojure-solutions.001_020])
  (:use [clojure.test])
  (:use [clojure.set])
  )

(deftest truth-001-test
  (is (= truth-001 true)))

(deftest math-002-test
  (is (= (- 10 (* 2 3)) math-002)))

(deftest strings-003-test
  (is (= strings-003 (.toUpperCase "hello world"))))

(deftest conj-005-test
  (is (= conj-005 (conj '(3 4) 2 1))))

(deftest conj-vec-007-test
  (is (= conj-vec-007 (conj [1 2] 3 4))))

(deftest sets-008-test
  (is (= sets-008 (set '(:a :a :b :c :c :c :c :d :d))))
  (is (= sets-008 (clojure.set/union #{:a :b :c} #{:b :c :d}))))

(deftest conj-009-test
  (is (= #{1 2 3 4} (conj #{1 4 3} conj-009))))

(deftest maps-010-test
  (is (= maps-010 ((hash-map :a 10, :b 20, :c 30) :b)))
  (is (= maps-010 (:b {:a 10, :b 20, :c 30}))))

(deftest conj-maps-011-test
  (is (= {:a 1, :b 2, :c 3} (conj {:a 1} conj-maps-011 [:c 3]))))

(deftest seq-012-test
  (is (= seq-012 (first '(3 2 1))))
  (is (= seq-012 (second [2 3 4])))
  (is (= seq-012 (last (list 1 2 3)))))

(deftest rest-013-test
  (is (= rest-013 (rest [10 20 30 40]))))

(deftest fn-014-test
  (is (= fn-014 ((fn add-five [x] (+ x 5)) 3)))
  (is (= fn-014 ((fn [x] (+ x 5)) 3)))
  (is (= fn-014 (#(+ % 5) 3)))
  (is (= fn-014 ((partial + 5) 3))))

(deftest double-015-test
  (is (= (double-015 2) 4))
  (is (= (double-015 3) 6))
  (is (= (double-015 11) 22))
  (is (= (double-015 7) 14)))

(deftest helloworld-016-test
  (is (= (helloworld-016 "Dave") "Hello, Dave!"))
  (is (= (helloworld-016 "Jenn") "Hello, Jenn!"))
  (is (= (helloworld-016 "Rhea") "Hello, Rhea!")))

(deftest seqmap-017-test
  (is (= seqmap-017 (map #(+ % 5) '(1 2 3)))))

(deftest seqfilter-018-test
  (is (= seqfilter-018 (filter #(> % 5) '(3 4 5 6 7)))))

(deftest last-019-test
  (is (= (last-019 [1 2 3 4 5]) 5))
  (is (= (last-019 '(5 4 3)) 3))
  (is (= (last-019 ["b" "c" "d"]) "d")))

(deftest penultimate-020-test
  (is (= (penultimate-020 (list 1 2 3 4 5)) 4))
  (is (= (penultimate-020 ["a" "b" "c"]) "b"))
  (is (= (penultimate-020 [[1 2] [3 4]]) [1 2])))

(run-tests)