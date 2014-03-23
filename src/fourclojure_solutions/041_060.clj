(ns fourclojure-solutions.041_060
  (:use [clojure.set]))

(def drop-nth-41
  #(->> (for [i (range 0 (count %) %2)]
          (->> % (drop i) (take (dec %2))))
        flatten))

(def factorial-42 #(->> % inc (range 1) (reduce *)))

(defn reverse-interleave-43 [col n]
  (->> col
       (map-indexed #(list (mod %1 n) %2))
       (sort-by first)
       (map #(drop 1 %))
       flatten
       (partition (/ (count col) n))))

(def rotate-44 #(->> %2 cycle (drop (mod % (count %2))) (take (count %2))))

;; 45: [1 4 7 10 13]

(def flip-46 #(fn [a b] (% b a)))

;; 47: 4

;; 48: 6

(def split-at-49 #(list (take % %2) (drop % %2)))

(def split-by-type-50 #(->> % (group-by class) (map second) set))

;; 51: [1 2 3 4 5]

;; 52: [2 4]

(defn longest-increasing-subseq-53
  [coll]
  (let [result
        (loop [mstreak '()
               streak [(first coll)]
               remaining (rest coll)]
          (let [fst (first remaining)
                rst (rest remaining)
                scnt (count streak)
                mcnt (count mstreak)]
            (cond (empty? remaining) (if (> scnt mcnt) streak mstreak)
                  (= (last streak) (dec fst)) (recur mstreak (conj streak fst) rst)
                  (< scnt mcnt) (recur mstreak [fst] rst)
                  :else (recur streak [fst] rst))))]
    (if (< 1 (count result)) result [])))

(def partition-54
  #(for [i (range 0
                  (->> %2 count (rem %) dec (- (count %2)))
                  %)]
    (->> %2 (drop i) (take %))))

(defn frequencies-55 [l]
  (into '{}
        (for [e (set l)]
          [e (count (filter #(= e %) l))])))

(def distinct-56
  #(loop [acc [] x % black '[]]
    (let [fst (first x)
          rst (rest x)]
      (cond (empty? x) acc
            (some #{fst} black) (recur acc rst black)
            :else (recur (conj acc fst) rst (conj black fst))))))

;; 57: [5 4 3 2 1]

(def composition-58
  (fn [& f]
    (fn [& args]
      (->> f reverse (reduce #(list (apply %2 %1)) args) first))))

(defn juxtaposition-59 [& ff] #(for [f ff] (apply f %&)))