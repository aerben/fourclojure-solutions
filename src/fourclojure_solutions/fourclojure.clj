(ns fourclojure-solutions.fourclojure)

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

(defn split49 "Solution 49. Split at n."
  [n lst]
  (list
    (take n lst) (drop n lst)))

(defn truthy83 "Solution 83. Variadic, takes booleans. Return if some are true and some are false."
  [& bools]
  (not (nil?
         (and
           (seq (drop-while true? bools))
           (seq (drop-while false? bools))))))