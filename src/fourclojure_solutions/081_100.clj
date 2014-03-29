(ns fourclojure-solutions.081_100
  (:use [clojure.set]))

(def intersection-81 #(set (filter % %2)))

(defn word-chains-82 [sq]
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