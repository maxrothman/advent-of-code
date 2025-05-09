(ns clj2024.day8
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]
            [medley.core :refer [indexed map-vals]]
            [clj2024.util :refer [spy]]))

(def test-raw
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn group-same-freq [raw]
  (->> (for [[y l] (indexed raw)
             [x c] (indexed l)
             :when (not= c \.)]
         [c [y x]])
       (group-by first)
       vals
       (map #(map second %))))

(defn bounds [raw]
  [(dec (count raw))
   (dec (count (first raw)))])

(defn in-bound [[ymax xmax] [y x]]
  (and (<= 0 y ymax) (<= 0 x xmax)))

(defn dist [[y1 x1] [y2 x2]]
  [(- y2 y1) (- x2 x1)])

(defn add [[y x] [dy dx]]
  [(+ dy y) (+ dx x)])

(defn antinodes1 [bnds pt1 pt2]
  (let [[pt1' pt2'] (sort-by first [pt1 pt2])
        dst (dist pt1' pt2')]
    (->> [(add pt1 (map - dst))
          (add pt2 dst)]
         (filter #(in-bound bnds %)))))

(defn antinodes2 [bnds pt1 pt2]
  (let [dst (dist pt1 pt2)]
    (for [f [#(add % dst) #(add % (map - dst))]
          node (take-while #(in-bound bnds %) (iterate f pt1))]
      node)))

(defn sln [antinodes raw]
  (let [lines (str/split-lines raw)
        bnds (bounds lines)
        grouped (group-same-freq lines)]
    (->> (for [pts grouped
               [pt1 pt2] (comb/combinations pts 2)
               node (antinodes bnds pt1 pt2)]
           node)
         distinct
         count)))

(comment
  (group-same-freq (str/split-lines test-raw))
  (let [pts (sort-by first [[5 5] [3 4]])]
    [(add (first pts) (map - (apply dist pts)))
     (add (second pts) (apply dist pts))])
  (antinodes [3 4] [5 5])
  (comb/combinations '([1 3] [2 0] [6 2] [7 6]) 2)
  (antinodes2 [9 9] [0 0] [1 3]) 
  (sln antinodes2 test-raw)

  (sln antinodes2 (slurp "resources/day8.txt"))
  )