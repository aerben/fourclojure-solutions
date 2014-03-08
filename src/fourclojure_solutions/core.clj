(ns fourclojure-solutions.core)

(defn interposition [value list]
  (flatten (reduce (fn [lst val] [lst value val]) list)))

(defn dropN [lst n]
  (reverse(loop [accum '() curlist lst curN n]
             (cond
               (empty? curlist) accum
               (= 1 curN) (recur accum (rest curlist) n)
               :else (recur (conj accum (first curlist)) (rest curlist) (dec curN))
               )
             )))