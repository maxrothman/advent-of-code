(ns clojure-aoc.day2 
  (:require [clojure.java.io :as io]))

(def test-input
  ["A Y"
   "B X"
   "C Z"])

;; A=X=rock
;; B=Y=paper
;; C=Z=scissors

(def winner-score
  {"A X" 3
   "B Y" 3
   "C Z" 3
   "A Y" 6
   "A Z" 0
   "B X" 0
   "B Z" 6
   "C X" 6
   "C Y" 0})

(def choice-score
  {\A 1
   \X 1
   \B 2
   \Y 2
   \C 3
   \Z 3})

(defn part1 [input]
  (+ (->> input
          (map winner-score)
          (reduce + 0))
     (->> input
          (map #(nth % 2))
          (map choice-score)
          (reduce + 0))))

(part1 test-input)
;; => 15

(def real-input
  (with-open [rdr (io/reader (io/resource "day2.txt"))]
   (vec (line-seq rdr))))

(part1 real-input)
;; => 8890


;; X=lose Y=draw Z=win
;; A=rock=1 B=paper=2 C=scissors=3

(def winner-score2
  {\X 0
   \Y 3
   \Z 6})

(def choice-score2
  {"A X" 3
   "A Y" 1
   "A Z" 2
   "B X" 1
   "B Y" 2
   "B Z" 3
   "C X" 2
   "C Y" 3
   "C Z" 1})

(defn part2 [input]
  (+ (->> input
          (map #(nth % 2))
          (map winner-score2)
          (reduce + 0))
     (->> input
          (map choice-score2)
          (reduce + 0))))

(part2 test-input)
;; => 12

(part2 real-input)
;; => 10238

