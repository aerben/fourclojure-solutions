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