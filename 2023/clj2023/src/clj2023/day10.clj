(ns clj2023.day10
  (:require [clj2023.util :refer [as->> spy until]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn up [[y x]] [(dec y) x])
(defn down [[y x]] [(inc y) x])
(defn left [[y x]] [y (dec x)])
(defn right [[y x]] [y (inc x)])

(defn parse [raw]
  (mapv vec (str/split-lines raw)))

(defn nbrs [grid c]
  (some->> (case (get-in grid c)
             \| [up down]
             \- [left right]
             \L [up right]
             \J [up left]
             \7 [left down]
             \F [right down]
             \S [up down left right]
             nil)
           (apply juxt)
           (as->> $ ($ c))))

(defn connected? [grid c1 c2]
  (and (some #{c2} (nbrs grid c1))
       (some #{c1} (nbrs grid c2))))

(defn neighbors [grid c]
  (->> (nbrs grid c)
       (remove #(contains? #{\.} (get-in grid %)))  ;Only pipes
       (filter (partial connected? grid c))))       ;If I'm not your nbr, you're not my nbr (pipes not aligned)

(defn explore [grid start]
  (:explored
   (until (comp empty? :to-explore)
          (fn go [{explored :explored
                   [[c dist] & to-explore] :to-explore}]
            {:explored (assoc explored c (min dist (explored c Integer/MAX_VALUE)))
             :to-explore (into (vec to-explore)
                               (->> (neighbors grid c)
                                    (filter (complement explored))
                                    (map #(vector % (inc dist)))))})
          {:explored {start 0}
           :to-explore (mapv vector
                             (neighbors grid start)
                             (repeat 1))})))

(defn find-start [grid]
  (->> (filter #(#{\S} (get-in grid %))
               (for [y (range (count grid))
                     x (range (count (first grid)))]
                 [y x]))
       first))

(defn pt1 [data]
  (let [d (parse data)]
    (->> (explore d (find-start d))
         (map second)
         (apply max))))

(def test-data
  ".....
.S-7.
.|.|.
.L-J.
.....")

(comment
  (neighbors (parse test-data) [3 3])

  (pt1 test-data)
  (pt1 (slurp (io/resource "day10.txt"))))

(comment
  (let [d (parse (slurp (io/resource "day10.txt")))]
    (->> (explore d (find-start d))
         (reduce #(assoc-in %1 (first %2) \▮) d)
         (map str/join)
         (str/join "\n")
         println)))

(defn even-odd-rule [loop y line]
  (reduce (fn [[inside? n] [x c]]
            (cond
              (and (loop [y x]) (#{\| \L \J} c)) [(not inside?) n]
              (and inside? (not (loop [y x]))) [inside? (inc n)]
              :else [inside? n]))
          [false 0]
          (map-indexed vector line)))

(defn pt2 [data]
  (let [d (parse data)
        loop-coords (explore d (find-start d))]
    (->> (map-indexed (partial even-odd-rule loop-coords) d)
         (map second)
         (reduce +))))

(def test-data
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(def test-data
  ".....
FS-7.
L7.|.
.L-J.
.....")
;; ".....
;; ┌--┐.
;; └┐.|.
;; .└-┘.
;; ....."

(comment
  (pt2 test-data)
  (pt2 (slurp (io/resource "day10.txt")))
  )
