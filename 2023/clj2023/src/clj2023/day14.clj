(ns clj2023.day14
  (:require [clj2023.util :refer [>>-> as->> resource-lines]]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [dom-top.core :refer [loopr]]))

(def test-data
  "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn parse [raw]
  (map vec raw))

(defn find-indexes [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn roll-1 [unmovable movable idx]
  (conj movable
        (let [[u m] [(first movable)
                     (first (subseq unmovable > idx))]]
             (if (and (nil? u) (nil? m))
               0
               (inc (max (or u 0)
                         (or m 0)))))))

(defn roll [row]
  (let [unmovable (into (sorted-set-by >) (find-indexes #{\#} row))
        movable (find-indexes #{\O} row)]
    (reduce (partial roll-1 unmovable)
            (sorted-set-by >)
            movable)))

(defn score [max-idx idxs]
  (->> (mapcat seq idxs)
       (map #(- max-idx %))
       (apply +)))

(defn pt1 [data]
  (let [parsed (parse data)
        max-idx (m/row-count parsed)]
    (->> (m/columns parsed)
         (map roll)
         (score max-idx))))

(comment
  (pt1 (str/split-lines test-data))
  (resource-lines "day14.txt" pt1)
  )

(defn sets->mat [m sets]
  (->> (map-indexed (fn [i s] (map #(vector % i) s)) sets)
       (mapcat identity)
       set
       (as->> coords
              (m/emap-indexed #(cond
                                 (= %2 \#) \#
                                 (coords %1) \O
                                 :else \.)
                              m))))

(defn sim-1 [m]
  (->> (map roll (m/columns m))
       (sets->mat m)))

(defn rotate [m]
  (map (comp vec reverse) (m/transpose m)))

(defn find-cycle [mat]
  (loopr [seen {}]
         [[i m] (map-indexed vector (iterate (comp rotate sim-1) mat))]
         (if-let [old-i (seen m)]
           [m old-i i]
           (recur (conj seen [m i])))))

(defn pt2 [cycles data]
  (let [parsed (parse data)
        max-idx (m/row-count parsed)
        iterations (* cycles 4)
        [cycle-start start end] (find-cycle parsed)
        nth-in-cycle (mod (- iterations start)
                          (- end start))]
    (prn start end)
    (->> (iterate (comp rotate sim-1) cycle-start)
         (>>-> nth nth-in-cycle)
         ;; Don't need to rotate m back into the right orientation because the target number of
         ;; cycles is hardcoded and divisible by 4
         m/columns
         (map #(find-indexes #{\O} %))
         (score max-idx))))

(comment
  (pt2 1000000000 (str/split-lines test-data))
  (time (resource-lines "day14.txt" (partial pt2 1000000000)))
  )