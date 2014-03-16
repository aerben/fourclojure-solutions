(ns fourclojure-solutions.fourclojure)

(defn flat_28
  "Flatten a sequence"
  [toFlat]
  (filter
    (complement sequential?)
    (tree-seq sequential? seq toFlat)))

(defn compress_30
  "Solution 30. Compress a sequence."
  [sq]
  (reduce
    (fn [cl val]
      (cond (= val (peek cl)) cl
            :else (conj cl val)))
    []
    sq))

(defn pack_31
  "Solution 31. Pack consecutive equal values into sublists."
  [coll]
  (partition-by identity coll))

(defn repeater_33
  "Solution 33. Repeat each element of a sequence n times and concatenate"
  [seq n]
  (mapcat identity (map #(repeat n %1) seq)))

(defn interposition_40
  "Solution 40. Interposition."
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
  "Solution 42. Calculate the factorial."
  [n]
  (reduce * (range 1 (inc n))))

(defn deinterleave_43
  "Solution 43: Reverse interleave"
  [col n]
  (let [size (/ (count col) n)]
    (partition size
               (flatten (map #(drop 1 %)
                             (sort-by first
                                      (map-indexed #(list (mod %1 n) %2) col)))))))

(defn rotate_44
  "Solution 44. Rotate."
  [n lst]
  (letfn [(rotate [direction n]
                  (cond (= :left direction)
                        (concat (take-last n lst) (take (- (count lst) n) lst))
                        :else
                        (concat (drop n lst) (take n lst))))]
    (if (> (Math/abs n) (count lst))
      (cond
        (neg? n) (rotate :left (- (Math/abs n) (count lst)))
        :else (rotate :right (- n (count lst))))
      (cond
        (neg? n) (rotate :left (Math/abs n))
        :else (rotate :right n)))))


(defn split_49
  "Solution 49. Split at n."
  [n lst]
  (list (take n lst) (drop n lst)))

(defn longest-increasing-subseq_53
  "Solution 53. Longest increasing subsequence above 1 element."
  [coll]
  (loop [maxstreak '()
         streak [(first coll)]
         remaining (rest coll)]
    (let [fst (first remaining)
          rst (rest remaining)
          streaklen (count streak) maxlen (count maxstreak)]
      (cond (empty? remaining) (cond (> streaklen maxlen) (if (> streaklen 1) streak [])
                                     :else (if (> maxlen 1) maxstreak []))
            (= (inc (last streak)) fst) (recur maxstreak (conj streak fst) rst)
            (< streaklen maxlen) (recur maxstreak [fst] rst)
            :else (recur streak [fst] rst)))))

(defn part_54
  "Solution 54. Partition."
  [n lst]
  (loop [cur lst acc '() streak '()]
    (let [streakcnt (count streak)]
      (cond (empty? cur) (cond
                           (= n streakcnt) (concat acc [streak])
                           :else acc)
            (= n streakcnt) (recur (rest cur) (concat acc [streak]) [(first cur)])
            :else (recur (rest cur) acc (concat streak [(first cur)]))))))

(defn countocc_55
  "Solution 55. Count occurences of entries in a sequence."
  [lst]
  (into '{}
        (for [setelement (set lst)]
          [setelement
           (count
             (filter #(= setelement %) lst))])))

(defn distinctn_56
  "Solution 56. Distinct."
  [lst]
  (reverse
    (loop [accum '() current lst blacklist '[]]
      (let [fst (first current) rst (rest current)]
        (cond (empty? current) accum
              (some #{fst} blacklist) (recur accum rst blacklist)
              :else (recur (conj accum fst) rst (conj blacklist fst)))))))

(defn composition_58
  "Solution 58. Composition."
  [& funs]
  (let [revfn (reverse funs)]
    (fn actual
      [& args]
      (loop
          [curres (apply (first revfn) args)
           nextfn (next revfn)]
        (if nextfn
          (recur
            ((first nextfn) curres)
            (next nextfn))
          curres)))))

(defn juxta_59
  "Solution 59. Juxtaposition."
  [& funs]
  (fn [& args]
    (for [fun funs]
      (apply fun args))))

(defn zip_61
  "Solution 61. Take a vector of keys and a vector of values, give a map interleaved and partitioned by 2."
  [keys values]
  (into '{}
        (for [entry (partition 2 (interleave keys values))]
          [(first entry) (second entry)])))

(defn group_by_63
  "Solution 63. Group by."
  [f cl]
  (apply merge-with concat (for [i cl] {(f i) [i]})))

(defn gcdEuclid_66
  "Solution 66. Greatest common divisor"
  [a, b]
  (loop [a a b b]
    (cond (= a b) a
          (> a b) (recur (- a b) b)
          :else (recur a (- b a)))))

(defn kingprimes_67
  "Solution 67. The king of primes selects the first n primes via regex."
  [n]
  (letfn [(prime? [x] (nil? (re-seq #"^1?$|^(11+?)\1+$" (apply str (repeat x "1")))))]
    (loop [accum [] i 2]
      (cond (= n (count accum)) accum
            (prime? i) (recur (conj accum i) (inc i))
            :else (recur accum (inc i))))))

(defn merge-with_69
  "Solution 69. Merge with."
  [f & maps]
  (letfn [(merge-item
            [mp entry]
            (let [k (first entry) v (second entry)]
              (if (contains? mp k)
                (merge mp {k (f (get mp k) v)})
                (conj mp entry))))
          (merge-maps
            [m1 m2]
            (loop [c1 m1 c2 m2]
              (cond (empty? c2) c1
                    :else (recur
                            (merge-item c1 (first c2))
                            (rest c2)))))]
    (reduce merge-maps maps)))

(defn wordsort_70
  "Solution 70. Sort words."
  [string]
  (sort-by (fn [x] (.toLowerCase x)) (re-seq #"\w+" string)))

(defn splitfilter_74
  "Solution 74. Split a string around commas and filter only perfect squares."
  [string]
  (let [res
        (filter
          (fn [e] (let [val (Math/sqrt (Integer/parseInt e))]
                    (= val (Math/floor val)))) (.split string ","))]
    (cond (< 1 (count res)) (str
                              (reduce (fn [val coll] (str val "," coll)) (butlast res))
                              ","
                              (last res))
          :else res)))

(defn truthy_83
  "Solution 83. Variadic, takes booleans. Return if some are true and some are false."
  [& bools]
  (not (nil? (and
               (seq (drop-while true? bools))
               (seq (drop-while false? bools))))))

(defn symmetricDiff_88
  "Solution 88. Symmetric difference of two sets."
  [s1 s2]
  (letfn
      [(disjn [set1 set2]
              (map
                #(cond (contains? set1 %1) nil :else %1)
                set2))]
    (set (filter
           (complement nil?)
           (concat (disjn s1 s2) (disjn s2 s1))))))

(defn bin-tree?_95
  "Solution 95. Checks if a binary tree is passed"
  [tree]
  (cond
    (not= 3 (count tree)) false
    :else (letfn [(check-tree
                    [subtree]
                    (cond (nil? subtree) true
                          (not (coll? subtree)) false
                          :else (bin-tree?_95 subtree)))]
            (let [lc (second tree)
                  rc (nth tree 2)]
              (and
                (check-tree lc)
                (check-tree rc))))))

(defn lcm_100
  "Solution 100. Least common multiple."
  [& args]
  (letfn [(gcd [a, b]
               (loop [a a b b]
                 (cond (= a b) a
                       (> a b) (recur (- a b) b)
                       :else (recur a (- b a)))))]
    (reduce
      (fn [a b]
        (/ (* a b) (gcd a b)))
      args)))

(defn take-while-n
  "Solution 114. Take while predicate matched less than n times."
  [n pred coll]
  (loop [cnt n cur coll acc '()]
    (let [fst (first cur)
          rst (rest cur)
          con (concat acc [(first cur)])]
      (cond
        (pred fst) (cond
                     (= cnt 1) acc
                     :else (recur (dec cnt) rst con))
        :else (recur cnt rst con)))))

(defn sumOfSquare_120
  "Solution 120. Count how many elements are smaller than the sum of their squared component digits."
  [lst]
  (letfn
      [(to-digits [i] (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}
                           (str i)))]
    (count
      (filter
        (fn [e]
          (< e
             (reduce + (map
                         (fn [sq] (* sq sq))
                         (to-digits e)))))
        lst))))

(defn pcards_128
  "Solution 128. Convert string representations of playing cards to keyword maps"
  [x]
  (let [ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}
        suits {\D :diamond \C :club \H :heart \S :spade}
        sq (seq x)]
    {:suit (suits (first sq))
     :rank (ranks (second sq))}))

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
         restlst (drop 2 lst)]
      (cond
        (> (count restlst) 1)
        (recur res restlst)
        :else res))))

(defn dotproduct_143
  "Solution 143. Dot product of two vectors."
  [v1 v2]
  (reduce
    +
    (map
      #(* (first %) (second %))
      (partition 2 (interleave v1 v2)))))

(defn oscillerate_144 [value & funs]
  "Solution 144. Oscillating iteration."
  (cons value
        (lazy-seq
          (apply oscillerate_144
                 ((first funs) value)
                 (concat (rest funs) [(first funs)])))))

(defn indexer_157
  "Index sequence entries"
  [seq]
  (reverse
    (loop [accum '() sq seq idx 0]
      (cond (empty? sq) accum
            :else (recur
                    (conj accum (list (first sq) idx))
                    (rest sq)
                    (inc idx))))))

(defn comparison_166
  "Solution 166. Takes a lower-than-operator and two operands, returns a keyword signalling the relationship."
  [ltOp fst snd]
  (cond (ltOp fst snd) :lt (ltOp snd fst) :gt :else :eq))