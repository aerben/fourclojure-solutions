(ns fourclojure-solutions.001_020
  (:use [clojure.set])
  )

(def truth-001 true)
(def math-002 4)
(def strings-003 "HELLO WORLD")
;; 004: :a :b :c
(def conj-005 '(1 2 3 4))

;; 006: :a :b :c
(def conj-vec-007 (vector 1 2 3 4))
(def sets-008 (set '(:a :b :c :d)))
(def conj-009 2)
(def maps-010 20)

(def conj-maps-011 {:b 2})
(def seq-012 3)
(def rest-013 [20 30 40])
(def fn-014 8)
(def double-015 #(* 2 %))

(def helloworld-016 #(str "Hello, " % "!"))
(def seqmap-017 [6 7 8])
(def seqfilter-018 [6 7])
(def last-019 #(first (reverse %)))
(def penultimate-020 #(second (reverse %)))