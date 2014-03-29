(ns fourclojure-solutions.081_100
  (:use [clojure.set]))

(def intersection-81
  #(set (filter % %2)))

(defn word-chains-82
  [sq]
  (letfn [(distance [s1 s2]
                    (cond
                      (empty? s1) (count s2)
                      (empty? s2) (count s1)
                      :else (min
                              (+ (if (= (first s1) (first s2)) 0 1)
                                 (distance (rest s1) (rest s2)))
                              (inc (distance (rest s1) s2))
                              (inc (distance s1 (rest s2))))))
          (iter [target level ele all found]
                (if (= target level)
                  (= target (count (distinct found)))
                  (->>
                    (filter #(= 1 (distance % ele)) all)
                    distinct
                    (#(for [e %] (iter target (inc level) e all (conj found ele)))))))]
    (let [l (into '() sq)]
      (->>
        (for [e l] (iter (count l) 0 e l [e]))
        flatten
        (some true?)
        nil? false?))))

(def half-true-83
  #(not (nil? (and (seq (drop-while true? %&)) (seq (drop-while false? %&))))))

(defn transitive-84
  [s]
  (let [step #(into % (for [[x1 y1] %
                            [x2 y2] %
                            :when (= y1 x2)]
                        [x1 y2]))
        t (step s)]
    (if (= (count t) (count s)) s (transitive-84 t))))

(defn powerset-85
  [s]
  (reduce
    (fn [x y]
      (set (concat x (map #(set (concat #{y} %)) x))))
    #{#{}}
    s))

(defn happy-number?-86 [n]
  (let [to-digits #(map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} (str %))]
    (loop [accum [] x n]
      (let [r (->> x
                   to-digits
                   (map #(* % %))
                   (reduce +))]
        (cond (= 1 r) true
              (some #(= r %) accum) false
              :else (recur (conj accum r) r))))))