(ns clj2023.day17
  (:require [arrangement.core :as order]
            [clj2023.util :refer [>>-> key-cmp spy]]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [flow-storm.api :as fs-api]
            [clojure.java.io :as io]))

(def test-data
  "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defn parse [data]
  (->> data
       str/split-lines
       (mapv #(str/split % #""))
       (m/emap parse-long)))

(defn neighbors [grid [cur :as path]]
  (let [prev-steps-in-dir (->> (partition 2 1 path)
                               #_reverse
                               (map #(apply mapv - %))
                               (partition-by identity)
                               first
                               ;; Don't run the rest of the path after 3, it doesn't matter after that
                               (take 3))]
    (->> (for [dir [[1 0] [-1 0] [0 1] [0 -1]]]
           (mapv + cur dir))
         (filter #(get-in grid %))  ;Don't go outside the grid
         (>>-> cond->>              ;No more than 2 steps in one direction
               (<= 3 (count prev-steps-in-dir))
               (remove (->> prev-steps-in-dir first (mapv + cur) (conj #{})))))))

(comment
  (m/pm @(def td (parse test-data)))
  (neighbors td '([0 0]))
  (neighbors td '([0 1] [0 0]))
  (neighbors td '([0 2] [0 1] [0 0]))
  (neighbors td '([5 0] [4 0] [3 0] [3 1] [2 1] [1 1] [1 0] [0 0]))
  (neighbors td '([1 4] [1 3] [1 2] [0 2] [0 1] [0 0])))

(defn explore-1 [grid [seen paths]]
  (let [[[wt _] path :as p] (first paths)
        rst (disj paths p)]
    [(conj seen (first path))
     (->> (neighbors grid path)
          (remove seen)
          (map #(vector [(+ wt (get-in grid %)) (rand)]
                        (conj path %)))
          (into rst))]))

(defn explore [grid start]
  (iterate (partial explore-1 grid)
           [#{} (sorted-set-by (key-cmp first) [[0 (rand)]
                                                (list start)])]))

(defn shortest-path [grid start end]
  (->> (explore grid start)
       (drop-while #(->> (second %)  ;paths (not seen)
                         first       ;first path
                         second      ;path (not wt)
                         first       ;p (most recent cell)
                         (not= end)))
       first   ;first matching
       second  ;paths (not seen)
       first   ;first path
       first   ;heat loss + tiebreaker
       first))

(defn render-path [grid path]
  (m/pm (reduce #(assoc-in %1 %2 "o") grid path)))

(comment
  (time (shortest-path td [0 0] [12 12]))

  (let [grid (parse (slurp (io/resource "day17.txt")))]
    (time (shortest-path grid [0 0] [30 30]#_[(count grid) (count (grid 0))])))
  )

(comment
  (fs-api/local-connect)
  (fs-api/instrument-namespaces-clj #{(str *ns*)})
  (require '[flow-storm.runtime.indexes.api :as index-api])
  (index-api/select-thread nil 21)
  (let [[flow-id thread-id] @index-api/selected-thread
        frames (index-api/all-frames flow-id thread-id
                                     (fn [fns fname _ _]
                                       (= "explore-1" fname)))]
    (filter #(->> %
                  :args-vec
                  second  ;second arg
                  second  ;paths
                  first   ;first path
                  second  ;path, not weight
                  first   ;current cell
                  (= [1 4]))
            frames)))

(comment
  (require '[clj-async-profiler.core :as prof])

  ;; Profile the following expression:
  (prof/profile (shortest-path (parse (slurp (io/resource "day17.txt"))) [0 0] [25 25]))

  ;; The resulting flamegraph will be stored in /tmp/clj-async-profiler/results/
  ;; You can view the HTML file directly from there or start a local web UI:

  (prof/serve-ui 8080) ; Serve on port 8080
  )
