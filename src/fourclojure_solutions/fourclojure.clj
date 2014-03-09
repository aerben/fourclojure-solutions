(ns fourclojure-solutions.fourclojure)

(defn flat_28
  "Flatten a sequence"
  [toFlat]
  (filter
    (complement sequential?)
    (tree-seq sequential? seq toFlat)))

(defn repeater_33
  "Solution 33. Repeat each element of a sequence n times and concatenate"
  [seq n]
  (mapcat identity
          (map
            #(repeat n %1)
            seq)))

(defn interposition_40
  "Solution 40. Interpose alternative."
  [value list]
  (flatten
    (reduce
      (fn [lst val] [lst value val]) list)))

(defn dropN_41
  "Solution 41. Drop nth element."
  [lst n]
  (reverse (loop [accum '() curlist lst curN n]
             (cond
               (empty? curlist) accum
               (= 1 curN) (recur accum (rest curlist) n)
               :else (recur (conj accum (first curlist)) (rest curlist) (dec curN))))))

(defn factorial_42
  "Solution 42. Calculate the factorial." [n]
  (reduce * (range 1 (inc n))))

(defn split_49
  "Solution 49. Split at n."
  [n lst]
  (list
    (take n lst) (drop n lst)))

(defn truthy_83
  "Solution 83. Variadic, takes booleans. Return if some are true and some are false."
  [& bools]
  (not (nil?
         (and
           (seq (drop-while true? bools))
           (seq (drop-while false? bools))))))

(defn symmetricDiff_88
  "Solution 88. Symmetric difference of two sets."
  [s1 s2]
  (letfn
      [(disjn [set1 set2]
              (map
                #(cond (contains? set1 %1) nil
                       :else %1)
                set2))]
    (set (filter
           (complement nil?)
           (concat (disjn s1 s2) (disjn s2 s1))))))

(defn sumOfSquare_120
  "Count how many elements are smaller than the sum of their squared component digits."
  [lst]
  (letfn
      [(to-digits
         [i]
         (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}
              (str i)))]
    (count
      (filter
        (fn [element]
          (< element
             (reduce
               +
               (map
                 (fn [sq] (* sq sq))
                 (to-digits element)))))
        lst))))

(defn infix_138
  "Solution 138. Parses arithmetic expression as infix, no operator precedence."
  [& args]
  (loop
      [acc (first args)
       lst (rest args)]
    (let
        [res (apply
               (first lst)
               [acc (second lst)])
         restlst (drop 2 lst)
         ]
      (cond
        (> (count restlst) 1)
        (recur res restlst)
        :else res
        ))))

(defn comparison_166
  "Solution 166. Takes a lower-than-operator and two operands, returns a keyword signalling the relationship."
  [ltOp fst snd]
  (cond (ltOp fst snd) :lt (ltOp snd fst) :gt :else :eq)
  )