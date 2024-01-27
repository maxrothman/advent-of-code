(ns clj2023.day17
  (:require [clj2023.util :refer [>>-> key-cmp spy]]
            [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [flow-storm.api :as fs-api]
            [clojure.data.priority-map :refer [priority-map]]))

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

(defn explore-1 [grid seen paths]
  (let [[path wt] (first paths)
        rst (pop paths)
        nbr-wts (->> (neighbors grid path)
                     (map #(vector ^{:path (conj path %)} % 
                                   (+ wt (get-in grid %))))
                     (filter #(< (second %) (seen (first %) Integer/MAX_VALUE))))]
    [(into seen nbr-wts)
     (into rst (for [[c w] nbr-wts]
                 [(conj path c) w]))]))

(defn explore [grid start]
  (iterate (partial apply explore-1 grid) 
           [{[0 0] 0} (priority-map (list start) 0)]))

(defn shortest-path [grid start end]
  (->> (explore grid start)
       (drop-while #(->> (second %)  ;paths (not seen)
                         first       ;first path
                         key         ;path (not wt)
                         first       ;p (most recent cell)
                         (not= end)))
       first   ;first matching
       second  ;paths (not seen)
       first   ;first path
       val     ;heat loss
       ))

(defn render-path [grid path]
  (m/pm (reduce #(assoc-in %1 %2 "o") grid path)))

(comment
  (->> (shortest-path td [0 0] [12 12]) first (render-path td))
  (time (shortest-path td [0 0] [12 12]))
  ;; I've got some kind of correctness issue :P 
  ;; The optimization of not visiting a space again once you've found the shortest path to it is
  ;; incorrect. The shortest path is path-dependent because of the
  ;; no-more-than-3-steps-in-the-same-dir thing, getting to a space "first" might deprive you of a
  ;; shorter global path if it would be faster to take an extra step in the same direction later
  ;; Should `seen` only get added to when we visit?
  ;; Should we not cull paths using the optimization at all?
  ;; Other slns take all the steps in a dir at once
  ;; It seems like that gets around the issue somehow?
  ;; In one sln, rather than tracking the best heat per space, it filters on the lowest heat
  ;; CURRENTLY BEING EXPLORED for a space, AND a new heat must be lower than the best previously
  ;; found heat for that space, but this doesn't include the current stretch.
  ;; I'm dubious of that optimization, maybe try at first without it?
  ;; Maybe I'm not understanding the problem correctly? That would surely cause the aforementioned
  ;; path-dependency issue, right?

  (m/pm td)
  (render-path td p)
  (->> '([0 4] [1 4] [1 3] [1 2] [0 2] [0 1] [0 0])
       butlast
       (map #(get-in td %))
       (apply +))

  (let [grid (parse (slurp (io/resource "day17.txt")))]
    (time (shortest-path grid [0 0] [30 30] #_[(count grid) (count (grid 0))])))
  )

(comment
  (fs-api/local-connect {:theme :dark})
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
            frames))
  )

(comment
  (require '[clj-async-profiler.core :as prof])

  ;; Profile the following expression:
  (prof/profile (shortest-path (parse (slurp (io/resource "day17.txt"))) [0 0] [25 25]))
  ;; "Elapsed time: 5539.046281 msecs"

  ;; The resulting flamegraph will be stored in /tmp/clj-async-profiler/results/
  ;; You can view the HTML file directly from there or start a local web UI:

  (prof/serve-ui 8080) ; Serve on port 8080
  )
