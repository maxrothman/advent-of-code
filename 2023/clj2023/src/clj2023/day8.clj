(ns clj2023.day8 
  (:require [clj2023.util :refer [resource-lines until]]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as nt]))

(defn parse [lines]
  (let [[[path-raw] [_ & nodes-raw]] (split-with (complement str/blank?) lines)
        path (cycle (map {\L first \R second} path-raw))
        nodes (->> (map #(re-seq #"[A-Z0-9]+" %) nodes-raw)
                   (map (juxt first rest))
                   (into {}))]
    [path nodes]))

(defn pt1 [[path nodes]]
  (until #(= "ZZZ" (first %))
         (fn [[node walked [d & unwalked]]] 
           (let [new-node (d (nodes node))]
             [new-node (conj walked new-node) unwalked]))
         ["AAA" [] path]))

(def test-data
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(comment
  (count (second (pt1 (parse (str/split-lines test-data)))))
  (resource-lines "day8.txt" (comp count second pt1 parse))
  )

(def test-data2
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn pt2-bruteforce [[path graph]]
  (until (fn [[nodes]] (every? #(str/ends-with? % "Z") nodes))
         (fn [[nodes steps [d & unwalked]]]
           (let [new-nodes (into #{} (map (comp d graph)) nodes)]
             [new-nodes (inc steps) unwalked]))
         [(set (filter #(str/ends-with? % "A") (keys graph)))
          0
          path]))

(defn find-route [start path graph]
  (until #(str/ends-with? (first %) "Z")
          (fn [[node steps [d & unwalked]]]
            (let [new-node (d (graph node))]
              [new-node (inc steps) unwalked]))
          [start 0 path]))

(defn route-lengths [[path graph]]
  (->> (map #(find-route % path graph)
            (filter #(str/ends-with? % "A") (keys graph)))
       (map second)))

(comment
  (reduce nt/lcm (route-lengths (parse (str/split-lines test-data2))))
  (resource-lines "day8.txt" (comp (partial reduce nt/lcm)
                                   route-lengths
                                   parse))
  )