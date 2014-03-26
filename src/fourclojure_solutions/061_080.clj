(ns fourclojure-solutions.061_080
  (:use [clojure.set]))

(def zipmap-61
  #(into '{}
         (for [[a b]
               (->> (interleave %1 %2) (partition 2))] [a b])))

(defn iterate-62 [f i] (lazy-seq (->> (f i) (iterate-62 f) (cons i))))

(def group-by-63 #(apply merge-with concat (for [i %2] {(%1 i) [i]})))

;; 64: +

(def blackbox-65
  #(cond (->> % empty (= '{})) :map
         (->> % empty (= '#{})) :set
         (->> :i (conj % 1) first (= :i)) :list
         :else :vector))

(def gcd-66
  #(cond (= %1 %2) %1
         (> %1 %2) (recur (- %1 %2) %1)
         :else (recur %1 (- %2 %1))))

(def prime?-67
  #(letfn [(p? [x] (->> (repeat x "1") (apply str) (re-seq #"^1?$|^(11+?)\1+$") nil?))]
    (loop [acc [] i 2]
      (cond (= % (count acc)) acc
            (p? i) (recur (conj acc i) (inc i))
            :e (recur acc (inc i))))))

;; 68:  (list 7 6 5 4 3)

(def merge-with-69
  #(letfn [(merge-item
             [mp [a b]]
             (if (contains? mp a)
               (merge mp {a (-> (get mp a) (% b))})
               (conj mp [a b])))
           (merge-maps [m1 m2]
                       (loop [c1 m1 c2 m2]
                         (if (empty? c2) c1
                                         (recur (merge-item c1 (first c2)) (rest c2)))))]
    (reduce merge-maps %&)))

(defn word-sorting-70 [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

;; 71: last

(def rearrange-72 #(reduce + %))

(defn tic-tac-toe-73 [board]
  (let [valid [5 50 149 45 66 93 80 56]]
    (->> board
         flatten
         (interleave (map #(Math/pow % 2) (range 9)))
         (partition 2)
         (group-by second)
         (map (fn [[a b]] (list a (reduce #(+ %1 (first %2)) 0 b))))
         (map (fn [[a b]] [a (some (hash-set (int b)) valid)]))
         (filter #(not (nil? (second %))))
         ffirst)))

(defn filter-perfect-squares-74 [string]
  (let [res (->> (.split string ",")
                 (map #(Integer/parseInt %))
                 (map #(Math/sqrt %))
                 (filter #(= % (Math/floor %)))
                 (map #(Math/pow % 2))
                 (map int))]
    (if (empty? res)
      res
      (->> res
           butlast
           (reduce #(str %1 "," %2))
           (#(str % "," (last res)))))))

(defn euler-totient-75 [x]
  (letfn [(gcd [x y]
               (cond (= x y) x
                     (> x y) (recur (- x y) x)
                     :else (recur x (- y x))))]
    (->> (for [i (range x)]
           (if (= 1 (gcd x i)) i))
         (filter #(not (nil? %)))
         count)))

;; 76: [1 3 5 7 9 11]

(defn find-anagram-77 [coll]
  (->> coll
       (group-by sort)
       (map second)
       (filter #(< 1 (count %)))
       (map #(apply hash-set %))
       (apply hash-set)))

(defn trampoline-78 [f v]
  (loop [fc (f v)]
    (if (ifn? fc) (recur (fc)) fc)))