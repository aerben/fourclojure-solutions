(ns fourclojure-solutions.test.061_080_test
  (:use [fourclojure-solutions.061_080])
  (:use [clojure.test])
  (:use [clojure.set]))

(deftest zipmap-61-test
  (is (= (zipmap-61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (zipmap-61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (zipmap-61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(deftest iterate-62-test
  (is (= (take 5 (iterate-62 #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (iterate-62 inc 0)) (take 100 (range))))
  (is (= (take 9 (iterate-62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(deftest group-by-63-test
  (is (= (group-by-63 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-by-63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-by-63 count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(deftest blackbox-65-test
  (is (= :map (blackbox-65 {:a 1, :b 2})))
  (is (= :list (blackbox-65 (range (rand-int 20)))))
  (is (= :vector (blackbox-65 [1 2 3 4 5 6])))
  (is (= :set (blackbox-65 #{10 (rand-int 5)})))
  (is (= [:map :set :vector :list] (map blackbox-65 [{} #{} [] ()]))))

(deftest gcd-66-test
  (is (= (gcd-66 2 4) 2))
  (is (= (gcd-66 10 5) 5))
  (is (= (gcd-66 5 7) 1))
  (is (= (gcd-66 1023 858) 33)))

(deftest prime?-67-test
  (is (= (prime?-67 2) [2 3]))
  (is (= (prime?-67 5) [2 3 5 7 11]))
  (is (= (last (prime?-67 100)) 541)))

(deftest merge-with-69-test
  (is (= (merge-with-69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (merge-with-69 - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (merge-with-69 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

(deftest word-sorting-70-test
  (is (= (word-sorting-70 "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting-70 "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting-70 "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

(deftest rearrange-72-test
  (is (= (rearrange-72 (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
         (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (rearrange-72))
         11)))

(deftest tic-tac-toe-73-test
  (is (= nil (tic-tac-toe-73 [[:e :e :e]
                              [:e :e :e]
                              [:e :e :e]])))
  (is (= :x (tic-tac-toe-73 [[:x :e :o]
                             [:x :e :e]
                             [:x :e :o]])))
  (is (= :o (tic-tac-toe-73 [[:e :x :e]
                             [:o :o :o]
                             [:x :e :x]])))
  (is (= nil (tic-tac-toe-73 [[:x :e :o]
                              [:x :x :e]
                              [:o :x :o]])))
  (is (= :x (tic-tac-toe-73 [[:x :e :e]
                             [:o :x :e]
                             [:o :e :x]])))
  (is (= :o (tic-tac-toe-73 [[:x :e :o]
                             [:x :o :e]
                             [:o :e :x]])))
  (is (= nil (tic-tac-toe-73 [[:x :o :x]
                              [:x :o :x]
                              [:o :x :o]]))))

(deftest filter-perfect-squares-74-test
  (is (= (filter-perfect-squares-74 "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares-74 "15,16,25,36,37") "16,25,36")))

(deftest euler-totient-75-test
  (is (= (euler-totient-75 99) 60))
  (is (= (euler-totient-75 40) 16))
  (is (= (euler-totient-75 10) (count '(1 3 7 9)) 4))
  (is (= (euler-totient-75 1) 1)))

(deftest find-anagram-77-test
  (is (= (find-anagram-77 ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (find-anagram-77 ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

(deftest trampoline-78-test
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop? (- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (trampoline-78 triple 2))
         82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial trampoline-78 my-even?) (range 6)))
         [true false true false true false])))

(comment (deftest triangle-minimum-path-79-test
  (is (= 7 (triangle-minimum-path-79 '([1]
                                       [2 4]
                                       [5 1 4]
                                       [2 3 4 5]))))
  (is (= 20 (triangle-minimum-path-79 '([3]
                                        [2 4]
                                        [1 9 3]
                                        [9 9 2 4]
                                        [4 6 6 7 8]
                                        [5 7 3 5 1 4]))))) )


(run-tests)