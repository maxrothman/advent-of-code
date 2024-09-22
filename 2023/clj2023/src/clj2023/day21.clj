(ns clj2023.day21 
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse [raw]
  (mapv #(str/split % #"") (str/split-lines raw)))

(comment
  @(def test-data
     (parse "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."))
  )

(defn neighbors [grid [y x]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (= 1 (+ (abs dx) (abs dy)))
        :let [[y' x'] [(+ y dy) (+ x dx)]]
        :when (#{"S" "."} (get-in grid [y' x']))]
    [y' x']))

(defn do-1 [grid locs]
  (into #{} (mapcat (partial neighbors grid)) locs))

(defn efind [m needle]
  (filter #(= needle (apply m/mget m %)) (m/index-seq m)))

(comment
  (-> (iterate (partial do-1 test-data) #{[5 5]})
      (nth 6)
      count
      #_(->> (reduce #(assoc-in %1 %2 "O") test-data)
             m/pm))
  (let [grid (parse (slurp (io/resource "day21.txt")))]
    (-> (iterate (partial do-1 grid) (into #{} (efind grid "S")))
           (nth 64)
           count))
  )
