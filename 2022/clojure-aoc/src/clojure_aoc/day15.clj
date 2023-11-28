(ns clojure-aoc.day15 
  (:require [clojure-aoc.util :refer [as->>]]
            [clojure.string :as str]
            [helins.interval.set :as iset]
            [clojure.java.io :as io]))

(def input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(->> (re-seq #"[-0-9]+" %)
                  (map parse-long)))
       (map (fn [[sx sy bx by]] {:sensor [sx sy] :beacon [bx by]}))))

(defn ->coverage [{:keys [sensor beacon]}]
  (zipmap [:x :y :range]
          (into sensor [(reduce + (map (comp abs -) sensor beacon))])))

(defn at-line [row-y {:keys [x y range]}]
  (let [from-center (abs (- row-y y))
        width (- range from-center)]
    (if (<= from-center range)
      [(- x width) (+ x width)]
      [])))

(defn part1 [line input]
  (let [parsed (parse input)
        beacons (->> (map :beacon parsed)
                     set
                     (filter #(= line (second %)))
                     (map first))]
    (->> parsed
         (map ->coverage)
         (map #(at-line line %))
         (filter seq)
         (map #(update % 1 inc))  ;iset wants right-exclusive ranges
         (reduce #(apply iset/mark %1 %2) iset/empty)
         (as->> ranges (reduce #(iset/erase %1 %2 (inc %2)) ranges beacons))
         (map (comp abs (partial apply -)))
         (reduce +))))

(comment
  (part1 2000000 (slurp (io/resource "day15.txt")))
  )

(defn distance [p1 p2]
  (reduce + (map (comp abs -) p1 p2)))

(defn farthest-corner [[x y xx yy] pt]
 (apply max-key (partial distance pt) [[x y] [xx yy] [x yy] [xx y]]))

(defn filled? [square sensors]
  (<= (distance (:pt s) (farthest-corner square (:pt s)))
      (:range s)))

;; .......
;; ..aaaa..
;; ..a..a..
;; ..a..a
;; ..aaaa..
;; .......
;;                1    1    2
;;      0    5    0    5    0
;; 0  ....S..................
;; 1  ......................S
;; 2  ...............S..X....
;; 3  .....aaaa.X.....SBX....
;; 4  .....a..a..X...X..X....
;; 5  .....a..a...X.X...X....
;; 6  .....aaaa....X....X....
;; 7  ..........S.X.X...S....
;; 8  ...........X.X....X....
;; 9  ............X.X...X....
;; 10 ....B......X..X.X.X....
;; 11 ..S.......X..X.XUXX....
;; 12 ............X...X.X....
;; 13 ...........X.....X.....
;; 14 ..........X...S.......S
;; 15 B..........X..X........
;; 16 ...........SB.X........
;; 17 ............XXX.S......
;; 18 ....S.......XXX........
;; 19 ............XXXX.......
;; 20 ............SXXXX..S...

;; How do I tell if a square is completely filled?
;; For a given point p, the point is filled if for any sensor,
;;   (dist p sensor) <= (range sensor)
;; A square is filled if for every point in the square, a sensor
;;   is within range
;; The point farthest away from a sensor is always a corner
;; 
;; Wait, what if 2 sensors work together to fill a square?
