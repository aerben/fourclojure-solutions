(ns fourclojure-solutions.021_040
  (:use [clojure.set]))

(def nth-21 #(->> %1 (drop %2) first))
(def count-22 #(->> % (map (constantly 1)) (reduce +)))
(def reverse-23 #(reduce conj '() %))
(def sum-24 #(reduce + %))
(def filter-odd-25 #(filter odd? %))

(def fib-n-26
  #(loop [a 1 b 1 c % r []]
    (if (= c 0) r (recur b (+ a b) (dec c) (conj r a)))))

(def palindrome-27
  #(= (reverse %) (seq %)))

(def flatten-28
  #(filter (complement sequential?)
           (tree-seq sequential? seq %)))

(def caps-29 #(reduce str (re-seq #"[A-Z]+" %)))
(defn compress-30 [s] (reduce #(if (= %2 (peek %)) % (conj % %2)) [] s))
(def pack-31 #(partition-by identity %))
(defn duplicate-32 [s] (->> s (map #(conj [%] %)) (mapcat identity)))
(defn replicate-33 [s n] (->> s (map #(repeat n %1)) (mapcat identity)))

(def range-34 #(take (- %2 %1) (iterate inc %1)))
(def local-35 7)
;; 36: [x 7,y 3,z 1])
(def regex-37 "ABC")
(defn max-38 [f & r] (reduce #(if (> %1 %2) %1 %2) f r))
;; 39: mapcat vector

(defn interpose-40 [v l]
  (->> l
       (reduce #(vector %1 v %2))
       flatten))
