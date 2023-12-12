(ns clj2023.day4
  (:require [clj2023.util :refer [>>-> map2 resource-lines]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [l]
  (->> (str/split l #":") second
       (>>-> str/split #"\|")
       (map #(re-seq #"(\d+)" %))
       (map2 second)
       (map set)))

(def point-sequence
  (conj (iterate #(* 2 %) 1) 0))

(defn score-line [l]
  (->> (parse-line l)
       (apply set/intersection)
       count))

(defn pt1 [lines]
  (->> (map (comp first #(drop % point-sequence) score-line) 
            lines)
       (reduce +)))

(comment
  (def parsed (parse-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
  (score-line "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
  (score-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
  (resource-lines "day4.txt" pt1)
  ;; => 25651
  )

(defn until 
  "Iterate f on x until (pred x) is true"
  [pred f x]
  (->> (iterate f x)
       (drop-while (complement pred))
       first))

(defn it [{[win & wins] :wins, [card & cards] :cards, processed :processed}]
  {:wins wins
   :cards (map + cards (-> (repeat win card)
                    (concat (repeat 0))))
   :processed (conj processed card)})

(defn pt2 [lines]
  (let [wins (map score-line lines)]
    (->> (until #(empty? (:cards %))
                it
                {:wins wins, :cards (repeat (count wins) 1), :processed []})
         :processed
         (reduce +))))

(comment
  (def test-data
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

  ; {card-num wins}
  @(def wins
     (->> (map score-line (str/split-lines test-data))))

  @(def cards (repeat (count wins) 1))
  (map + cards (concat (repeat (val (first wins)) 1) (repeat 0)))
  (reduce (fn)
          (map vector wins cards))
  
  (let [wins (map score-line (str/split-lines test-data))]
    #_(->> (iterate it {:wins wins, :cards (repeat (count wins) 1), :processed []})
         (take 3))
    (->> (until #(empty? (:cards %))
                it
                {:wins wins, :cards (repeat (count wins) 1), :processed []})
         :processed
         (reduce +)))

  (resource-lines "day4.txt" pt2)
  )

