(ns clojure-aoc.day19 
  (:require [clojure-aoc.util :refer [spy]]
            [clojure.string :as str]))

(def input
  "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn parse [input]
  (map (fn [l]
         (let [id (-> (re-find #"^Blueprint (\d+):" l)
                      second
                      parse-long)
               itms (re-seq #"Each ([a-z]+) robot costs (\d+) ([a-z]+)(?: and (\d+) ([a-z]+))*\." l)]
           {id
            (into {}
                  (map (fn [[_ name & costs]]
                         [name
                          (->> (remove nil? costs)
                               (partition 2)
                               (map vec)
                               (map #(update % 0 parse-long)))])
                       itms))}))
       (str/split-lines input)))

;; optimizations:
;; - if you have more of a resource than you could spend in the remaining steps, you have "infinite"
;;   and all paths under that in the search tree are the same
;; - Don't explore paths that can't lead to any geodes
;;   - e.g. if you're at fewer steps left than the obsidian required to build a geode robot, you
;;     won't end up with any geodes
;; 
;; This seems interesting:
;; https://www.reddit.com/r/adventofcode/comments/zpnkbm/comment/j0v2d46/?utm_source=reddit&utm_medium=web2x&context=3
;; Paired with an initial search that finds a solutino with at least 1 geode (if such a solution
;; exists) would prune all the 0 paths
;; 
;; https://www.reddit.com/r/adventofcode/comments/zpy5rm/comment/j0vk97a/?utm_source=reddit&utm_medium=web2x&context=3
