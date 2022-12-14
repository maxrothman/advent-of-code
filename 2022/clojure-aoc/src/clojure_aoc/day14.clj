(ns clojure-aoc.day14 
  (:require [clojure-aoc.util :refer [as->> over]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn visualize [world]
  (println)
  (let [[points symbols] (->> world
                              (map (fn [[k v]] (vector k ({:rock \# :sand \.} v))))
                              (apply map vector))
        [xmin xmax] ((juxt #(apply min %) #(apply max %)) (map first points))
        [ymin ymax] ((juxt #(apply min %) #(apply max %)) (map second points))
        scaled-points (map (fn [[x y]] [(- x xmin) (- y ymin)]) points)]
    (->> (vec (repeat (- (inc ymax) ymin) (vec (repeat (- (inc xmax) xmin) " "))))
         (as->> grid (reduce #(apply assoc-in %1 %2)
                             grid
                             (map vector (map reverse scaled-points) symbols)))
         (map str/join)
         (str/join "\n")
         println)))

(def input
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn pair-coords [[x1 y1] [x2 y2]]
  (for [x (let [[xmin xmax] (sort [x1 x2])] (range xmin (inc xmax)))
        y (let [[ymin ymax] (sort [y1 y2])] (range ymin (inc ymax)))]
    [x y]))

(defn line-coords [line]
  (mapcat (partial apply pair-coords) (partition 2 1 line)))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #" -> "))
       (map (partial map #(str/split % #",")))
       (map (partial map (partial map parse-long)))
       (mapcat line-coords)
       (reduce #(assoc %1 %2 :rock) {})))
(comment
  (visualize (parse input))
  )

(defn step [world [x y]]
  (cond
    (nil? (world [x (inc y)])) [x (inc y)]
    (nil? (world [(dec x) (inc y)])) [(dec x) (inc y)]
    (nil? (world [(inc x) (inc y)])) [(inc x) (inc y)]
    :else [x y]))

(defn simulate-1 [world [start-x start-y]]
  (let [max-y (->> world keys (map second) (apply max))
        [x y] (->> (iterate #(step world %) [start-x (dec start-y)])
                   (partition 2 1)
                   (drop-while (fn [[old-coord new-coord]]
                                 (and (not= old-coord new-coord)
                                      (<= (second new-coord) max-y))))
                   first
                   second)]
    (if (> y max-y)
      world
      (assoc world [x y] :sand))))

(defn part1 [world start]
  (->> (reductions simulate-1 world (repeat start))
       (partition 2 1)
       (take-while #(apply not= %))
       count))
(comment
  (->> (reduce simulate-1 (parse input) (repeat 25 [500 0]))
       visualize)
  (part1 (parse input) [500 0])
  ;; => 24
  (part1 (parse (slurp (io/resource "day14.txt"))) [500 0])
  )

(defn simulate-2 [max-y world [start-x start-y]]
  (let [[x y] (->> (iterate #(step world %) [start-x (dec start-y)])
                   (partition 2 1)
                   (drop-while (fn [[old-coord new-coord]]
                                 (and (not= old-coord new-coord)
                                      (<= (second new-coord) max-y))))
                   first
                   second)]
    (if (>= y max-y)
      (assoc world [x max-y] :sand)
      (assoc world [x y] :sand))))

(defn part2 [world start]
  (let [max-y (->> world keys (map second) (apply max) inc)]
   (->> (reductions (partial simulate-2 max-y) world (repeat start))
        (take-while #(nil? (get % start)))
        last
        visualize)))
(comment
  (part2 (parse input) [500 0])
  ;; => 93
  (part2 (parse (slurp (io/resource "day14.txt"))) [500 0])
  ;; => 25434

  )