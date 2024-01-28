(ns clj2023.day18 
  (:require [clj2023.util :refer [spy]]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-data
  "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(defn parse [raw]
  (->> (str/split-lines raw)
       (mapcat #(re-seq #"([UDLR]) (\d+)" %))
       (map (comp vec rest))
       (map #(update % 1 parse-long))))

(defn dig [instrs]
  (first 
   (reduce (fn [[seen coord] [dir n]]
             (let [dirv (case dir "R" [0 1], "L" [0 -1], "U" [-1 0], "D" [1 0])
                   coords (->> (range 1 (inc n))
                               (map #(mapv (partial * %) dirv))
                               (map #(mapv + coord %)))]
               [(into seen coords)
                (last coords)]))
           ['() [0 0]]
           instrs)))

(defn dig-sparse [instrs]
  (reduce (fn [[coord & _ :as poly] [dir n]]
            (let [dirv (case dir "R" [1 0], "L" [-1 0], "U" [0 1], "D" [0 -1])]
              (conj poly (mapv + coord (mapv (partial * n) dirv)))))
          '([0 0])
          instrs))

(defn render [coords]
  (let [min-y (apply min (map second coords))
        max-y (apply max (map second coords))
        min-x (apply min (map first coords))
        max-x (apply max (map first coords))]
    (prn "mins" min-x min-y)
    (m/pm (reduce #(m/mset %1 (- (first %2) min-x) (- (second %2) min-y) :X)
                  (m/reshape [[]] (spy [(- max-x min-x -1) (- max-y min-y -1)]))
                  coords))))

(defn n-boundary-pts [pts]
  (->> (partition 2 1 pts)
       (map (fn [[[x1 y1] [x2 y2]]] (abs (+ 1 (- y2 y1) (- x2 x1)))))
       (apply +)))

(defn area [pts]
  ;; Combines trapezoid formulation of shoelace formula to get geometric area with Pick's theorem to
  ;; get from geometric area to number of enclosed points
  ;; Shoelace formula: A=|½∑(yᵢ+yᵢ₊₁)(xᵢ-xᵢ₊₁)|
  ;; Pick's theorem: A=i+b/2-1
  ;; Combined: i = 1 - b/2 + |½∑(yᵢ+yᵢ₊₁)(xᵢ-xᵢ₊₁)| 
  (let [boundary-pts (n-boundary-pts pts)]
    (->> (partition 2 1 pts)
         ;; Shoelace formula
         (map (fn [[[x1 y1] [x2 y2]]] (* (+ y1 y2) (- x1 x2))))
         (apply +)
         (* 1/2)
         abs
         ;;  Pick's theorem
         (+ 1 (- (quot boundary-pts 2)))
         ;; Add the boundary pts back in
         (+ boundary-pts))))

(defn parse2 [raw]
  (->> (str/split-lines raw)
       (mapcat #(re-seq #"\(#([0-9a-f]+)([0-9a-f])\)" %))
       (map #(vector (nth % 2) (nth % 1)))
       (map #(update % 1 (fn [x] (Long/parseLong x 16))))
       (map #(update % 0 (fn [x] (case x "0" "R" "1" "D" "2" "L" "3" "U"))))))

(comment
  (render (dig (parse test-data)))
  (area (dig-sparse (parse test-data)))
  (area (dig-sparse (parse (slurp (io/resource "day18.txt")))))
  ;; This is off by 1 for some reason, sub 1 for correct answer

  (area (dig-sparse (parse2 test-data)))
  (area (dig-sparse (parse2 (slurp (io/resource "day18.txt")))))
  )