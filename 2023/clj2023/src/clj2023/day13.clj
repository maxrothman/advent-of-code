(ns clj2023.day13
  (:require [clj2023.util :refer [resource-lines]]
            [clojure.core.matrix :as m]
            [clojure.string :as str]))

(def test-data
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn ->matrix [lines]
  (mapv vec lines))

(defn trim [[pre post]]
  (let [smaller (min (count pre) (count post))]
    [(reverse (take smaller (reverse pre)))
     (take smaller post)]))

(defn find-reflection-1 [vecs]
  (->> (map #(split-at % vecs) (range 1 (count vecs)))
       (map trim)
       (map #(update % 0 reverse))
       (map-indexed vector)
       (filter (comp (partial apply =) second))
       first
       first))

(defn find-reflection [m]
  ;; inc b/c we didn't start our map-indexed at rank 0, but between rank 0 and rank 1
  (first
   (filter some?
           [(some-> (m/rows m) find-reflection-1 inc (* 100))
            (some-> (m/columns m) find-reflection-1 inc)])))

(defn parse [lines]
  (->> (partition-by empty? lines)
       (filter #(< 1 (count %)))
       (map ->matrix)))

(defn pt1 [lines]
  (->> (map find-reflection
            (parse lines))
       (filter some?)
       (apply +)))

(comment
  (pt1 (str/split-lines test-data))
  (resource-lines "day13.txt" pt1)
  )

;; Pt 2: rather than trying to change every cell and look for reflections, find the diffs between
;; each possible reflection, look for diff xize == 1
;; Or better yet, m/eq them, then sum the result

(defn neq [a b]
  (m/emap #(if (not= %1 %2) 1 0) a b))

(defn find-reflection-1-2 [vecs]
  (->> (map #(split-at % vecs) (range 1 (count vecs)))
       (map trim)
       (map #(update % 0 reverse))
       (mapv (comp (partial apply +) m/eseq (partial apply neq)))
       (map-indexed vector)
       (filter (comp #(= 1 %) second))
       first
       first))

(defn find-reflection2 [m]
  ;; inc b/c we didn't start our map-indexed at rank 0, but between rank 0 and rank 1
  (first
   (filter some?
           [(some-> (m/rows m) find-reflection-1-2 inc (* 100))
            (some-> (m/columns m) find-reflection-1-2 inc)])))

(defn pt2 [lines]
  (->> (map find-reflection2
            (parse lines))
       (filter some?)
       (apply +)))

(comment
  (pt2 (str/split-lines test-data))
  (resource-lines "day13.txt" pt2)
  )