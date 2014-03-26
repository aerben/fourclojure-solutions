(ns fourclojure-solutions.fourclojure)

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

(defn equivalence-class-98
  "Solution 98. Computate equivalence classes"
  [f coll]
  (->> coll
       (group-by f)
       (map #(into '#{} (second %)))
       (into '#{})))


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