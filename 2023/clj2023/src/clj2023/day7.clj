(ns clj2023.day7
  (:require [clj2023.util :refer [as->> resource-lines]]
            [clojure.string :as str]))

(defn card->n [c]
  (get {\T 10, \J 11, \Q 12, \K 13, \A 14}
       c
       (parse-long (str c))))

(defn rank
  "7 -> 5 of a kind, 6 -> 4 of a kind, ... 1 -> high card"
  [hand]
  (let [freqs (frequencies (vals (frequencies hand)))]
    (into [(cond
             (freqs 5) 7
             (freqs 4) 6
             (and (freqs 3) (freqs 2)) 5
             (freqs 3) 4
             (= 2 (freqs 2)) 3
             (freqs 2) 2
             :else 1)]
          hand)))

(comment
  (rank (mapv card->n "QQQJA"))
  (sort-by identity #(compare %2 %1) [[2 1 1 2 3] [5 1 2 1 1] [1 2 1 1 1]])
  (parse ["QQQJA 123" "T55J5 321"])
  )

(defn parse [lines]
  (->> (map #(str/split % #" ") lines)
       (map (fn [[hand bid]]
              [(rank (mapv card->n hand))
               (parse-long bid)]))))

(defn sol [ranked]
  (->> (sort-by first ranked)
      ;;  (as->> $ (= (count $) (count (set $))))
      ;;  (map-indexed #(vector %1 %2))
       (map-indexed (fn [i [_ bid]] (* bid (inc i))))
      (reduce +)))

(def test-data
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(comment
  (sol (parse (str/split-lines test-data)))
  (resource-lines "day7.txt" (comp sol parse))
  )

(defn card->n2 [c]
  (get {\T 10, \Q 12, \K 13, \A 14, \J 1}
       c
       (parse-long (str c))))

(defn rank2
  "7 -> 5 of a kind, 6 -> 4 of a kind, ... 1 -> high card"
  [hand]
  (let [freqs-with-jokers (frequencies hand)
        jokers (freqs-with-jokers 1 0)
        freqs (frequencies (vals (dissoc freqs-with-jokers 1)))]
    (into [(cond
             (or (= 5 jokers) (freqs (- 5 jokers))) 7
             (freqs (- 4 jokers)) 6
             (or (and (freqs 3) (freqs 2))
                 (and (= 2 (freqs 2)) (= 1 jokers))) 5
             (freqs (- 3 jokers)) 4
             (= 2 (freqs 2)) 3
             (freqs (- 2 jokers)) 2
             :else 1)]
          hand)))

(defn parse2 [lines]
  (->> (map #(str/split % #" ") lines)
       (map (fn [[hand bid]]
              [(rank2 (mapv card->n2 hand))
               (parse-long bid)]))))

(comment
  (sol (parse2 (str/split-lines test-data)))
  (resource-lines "day7.txt" (comp sol parse2))
  )