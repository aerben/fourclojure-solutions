(ns fourclojure-solutions.fourclojure)

(defn flat_28 "Flatten a sequence"
  [toFlat]
  (filter
    (complement sequential?)
    (tree-seq sequential? seq toFlat)))

(defn repeater_33 "Solution 33. Repeat each element of a sequence n times and concatenate"
  [seq n]
  (mapcat identity
          (map
            #(repeat n %1)
            seq)))

(defn interposition_40 "Solution 40. Interpose alternative."
  [value list]
  (flatten
    (reduce
      (fn [lst val] [lst value val]) list)))

(defn dropN_41 "Solution 41. Drop nth element."
  [lst n]
  (reverse (loop [accum '() curlist lst curN n]
             (cond
               (empty? curlist) accum
               (= 1 curN) (recur accum (rest curlist) n)
               :else (recur (conj accum (first curlist)) (rest curlist) (dec curN))))))

(defn factorial_42 "Solution 42. Calculate the factorial." [n]
  (reduce * (range 1 (inc n))))

(defn split_49 "Solution 49. Split at n."
  [n lst]
  (list
    (take n lst) (drop n lst)))



(defn truthy_83 "Solution 83. Variadic, takes booleans. Return if some are true and some are false."
  [& bools]
  (not (nil?
         (and
           (seq (drop-while true? bools))
           (seq (drop-while false? bools))))))