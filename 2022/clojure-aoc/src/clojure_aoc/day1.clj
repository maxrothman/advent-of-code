(ns clojure-aoc.day1 
  (:require [clojure-aoc.util :as util]
            [clojure.string :as str]))

@(def input 
   (as-> (util/read-input "day1") $
     (str/split $ #"\n\n")
     (map #(str/split % #"\n") $)))

@(def part1
   (->> input
        (util/map2 parse-long)
        (map #(reduce + %))
        (sort util/descending)
        (apply max)))
  ;; => 68775
   
@(def part2
   (->> input
        (util/map2 parse-long)
        (map #(reduce + %))
        (sort util/descending)
        (take 3)
        (reduce +)))
;; => 202585
