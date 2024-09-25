(ns clj2023.day24
  (:require [clj2023.util :refer [>>-> resource-lines]]
            [clojure.math.combinatorics :as combos]
            [clojure.string :as str]))

;; 2 lines always intersect if they are not parallel
;; How to tell where their intersections occur?
;; m1*x + b1 = m2*x + b2
;; x = (b2 - b1)/(m1 - m2)
;; y = m1*x + b1 (or m2/b2, doesn't matter)

;; Sometimes though, they crossed in the past (skip those)
;; Where y1 > y2: if slope1 - slope2 = 0, they're parallel
;;                if slope1 - slope2 > 0, they're diverging (crossed in the past)
;;                if slope1 - slope2 < 0, they're converging (will cross, COUNT THESE!)

(def test-raw
  "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defn parse [line]
  (let [[x y _ vx vy _] (re-seq #"-?\d+" line)]
    (update-vals {:x x, :y y, :vx vx, :vy vy}
                 parse-long)))

(defn slope [pt]
  ;; b = y - mx
  (let [slope (/ (:vy pt) (:vx pt))]
    (assoc pt
           :slope slope
           :b (- (:y pt) (* slope (:x pt))))))

(defn will-intersect? [[lo-bound hi-bound] [pt1 pt2]]
  (let [[smaller bigger] (sort-by :b [pt1 pt2])
        ixn-x (delay (/ (- (:b bigger) (:b smaller)) (- (:slope smaller) (:slope bigger))))
        ixn-y (delay (+ (:b smaller) (* (:slope smaller) @ixn-x)))]
    (when (and (> 0 (- (:slope bigger) (:slope smaller)))  ;not parallel, have a chance of intersecting in x>0
               (< lo-bound @ixn-x hi-bound)  ;x intersection in bounds
               (< lo-bound @ixn-y hi-bound)  ;y intersection in bounds
               (every? #(= (pos? (- @ixn-x (:x %)))  ;ixns happen in the "future"
                           (pos? (:vx %)))        ;NB: "future" is in slope direction
                       [pt1 pt2]))
      [pt1 pt2 ])))

(defn pt1 [bounds lines]
  (->> lines
       (map parse)
       (map slope)
       (>>-> combos/combinations 2)
       (map (partial will-intersect? bounds))
       (filter identity)
       count))

(comment
  (pt1 [7 27] (str/split-lines test-raw))
  ;; => 2

  (resource-lines "day24.txt" (partial pt1 [200000000000000 400000000000000]))
  ;; => 13754
  )
