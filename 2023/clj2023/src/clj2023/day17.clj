(ns clj2023.day17
  (:require [clj2023.util :refer [>>-> key-cmp spy]]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [flow-storm.api :as fs-api]
            [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

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

(defn neighbors [grid [{:keys [coord dir steps path] :as pp} {:keys [wt]}]]
  (for [dir' [[1 0] [-1 0] [0 1] [0 -1]]
        :when (not= dir' (mapv - dir))  ;don't go backwards
        :let [coord' (mapv + coord dir')]
        :when (get-in grid coord')  ;don't go outside the grid
        :let [steps' (if (= dir dir') (inc steps) 1)]
        :when (<= steps' 3)]
    [{:coord coord', :dir dir', :steps steps', :wt (+ wt (get-in grid coord')), :path (hash [coord' path])}
     {:wt (+ wt (get-in grid coord'))}]))

(comment
  (m/pm @(def td (parse test-data)))
  (neighbors td [{:coord [0 0] :dir [1 0]} {:wt 0 :steps 0}])
  ;; => ([1 0] [0 1])
  (neighbors td [{:coord [0 1] :dir [0 1]} {:wt 4 :steps 1}])
  ;; => ([1 1] [0 2])
  (neighbors td [{:coord [0 2] :dir [0 1]} {:wt 5 :steps 2}])
  ;; => ([1 2] [0 3])
  (neighbors td [{:coord [5 0] :dir [1 0]} {:wt 100 :steps 2}])
  ;; => ([6 0] [5 1])
  (neighbors td [{:coord [1 4] :dir [0 1]} {:wt 100 :steps 2}])
  ;; => ([2 4] [0 4] [1 5])
  (neighbors td [{:coord [3 0] :dir [1 0]} {:wt 100 :steps 3}])
  ;; => ([{:coord [3 1], :dir [0 1]} {:wt 104, :steps 0}]) 
  )

(defn explore-1 [grid [seen paths]]
  (let [[{:keys [coord]} _ :as p] (first paths)
        rst (pop paths)]
    [(conj seen coord)
     (into rst
           (remove (comp seen :coord first))
           (neighbors grid p))]))

(defn shortest-path [grid start end]
  (loop [[seen paths] [#{}
                       (priority-map-keyfn :wt
                        ^{:p (list start)} {:coord start, :dir [1 0], :steps 0, :wt 0, :path (hash start)} {:wt 0}
                        ^{:p (list start)} {:coord start, :dir [0 1], :steps 0, :wt 0, :path (hash start)} {:wt 0})]]
    (if (= end (:coord (key (first paths))))
      (do (prn (:p (meta (key (first paths)))))
          (:wt (val (first paths))))
      (recur (explore-1 grid [seen paths])))))

(defn render-path [grid path]
  (m/pm (reduce #(assoc-in %1 %2 "o") grid path)))

(comment
  (time (shortest-path td [0 0] [12 12]))

  (render-path td '([12 12] [12 11] [11 11] [10 11] [9 11] [9 12] [8 12] [7 12] [7 11] [6 11] [5 11] [5 10] [4 10] [3 10] [3 9] [2 9] [2 8] [1 8] [0 8] [0 7] [0 6] [1 6] [1 5] [1 4] [1 3] [0 3] [0 2] [0 1] [0 0]))

  (let [grid (parse (slurp (io/resource "day17.txt")))]
    (time (shortest-path grid [0 0] [(dec (count grid)) (dec (count (grid 0)))])))
  ;; 714 is wrong somehow, it's too high. I give up.
  ;; If I add the path to the key, it's too slow. wtf.
  ;; It's 4x slower with paths in the key, probably because I need more culling or A* or something
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

  ;; The resulting flamegraph will be stored in /tmp/clj-async-profiler/results/
  ;; You can view the HTML file directly from there or start a local web UI:

  (prof/serve-ui 8080) ; Serve on port 8080
  )
