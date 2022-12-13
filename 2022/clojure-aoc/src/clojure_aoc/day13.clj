(ns clojure-aoc.day13 
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.match :as match]
            [clojure.java.io :as io]))

(def input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map #(map edn/read-string %))))

(defn cmp [left right]
  (match/match [left right]
    [[] []] 0
    [[] [_ & _]] -1
    [[_ & _] []] 1

    [[(l :guard int?) & ls] [(r :guard vector?) & rs]]
    (recur (into [[l]] ls) (into [r] rs))

    [[(l :guard vector?) & ls] [(r :guard int?) & rs]]
    (recur (into [l] ls) (into [[r]] rs))

    [[(l :guard int?) & ls] [(r :guard int?) & rs]]
    (if (= 0 (compare l r))
      (recur ls rs)
      (compare l r))

    [[(l :guard vector?) & ls] [(r :guard vector?) & rs]]
    (let [c (cmp l r)]
      (if (= 0 c) (recur ls rs) c))))

(def answers [-1 -1 1 -1 1 -1 1 1])

(defn part1 [input]
  (->> (parse input)
       (map #(apply cmp %))
       (map vector (range 1 9999))
       (filter #(= -1 (second %)))
       (map first)
       (apply +)))
(comment
  (part1 input)
  ;; => 13
  (part1 (slurp (io/resource "day13.txt")))
  ;; => 6070
  )

(defn part2 [input]
  (->> (parse input)
       (mapcat identity)
       (into [[[2]] [[6]]])
       (sort cmp)
       (map vector (range 1 9999))
       (filter #(some #{[[2]] [[6]]} %))
       (map first)
       (apply *)))
(comment
  (part2 (slurp (io/resource "day13.txt")))
  ;; => 20758

  )