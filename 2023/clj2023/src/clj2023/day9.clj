(ns clj2023.day9 
  (:require [clj2023.util :refer [resource-lines]]
            [clojure.string :as str]))

(defn parse [line]
  (map parse-long (re-seq #"[\d-]+" line)))

(defn next-in-seq [s]
  (if (= 1 (count (set s)))
    (first s)
    (+ (last s) 
       (next-in-seq (->> (partition 2 1 s) 
                         (map #(apply - (reverse %))))))))

(defn sol [extrapolator lines]
  (apply +
         (map (comp extrapolator parse)
              lines)))

(def test-data
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(comment
  (sol next-in-seq (str/split-lines test-data))
  (resource-lines "day9.txt" (partial sol next-in-seq))
  )

(defn prev-in-seq [s]
  (if (= 1 (count (set s)))
    (first s)
    (- (first s)
       (prev-in-seq (->> (partition 2 1 s)
                         (map #(apply - (reverse %))))))))

(comment
  (sol prev-in-seq (str/split-lines test-data))
  (resource-lines "day9.txt" (partial sol prev-in-seq))
  )