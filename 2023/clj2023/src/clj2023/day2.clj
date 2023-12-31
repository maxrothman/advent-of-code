(ns clj2023.day2
  (:require [clj2023.util :refer [map2 resource-lines spy]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-data
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-line [l]
  (let [[game moves] (str/split l #":")
        id (->> game (re-matches #"Game (\d+)") second parse-long)]
    [id
     (->> (str/split moves #";")
          (map #(str/split % #","))
          (map2 #(->> %
                      str/trim
                      (re-matches #"(\d+) ([a-z]+)")
                      rest
                      reverse
                      vec))
          (map #(into {} %))
          (map #(-> %
                    (update-keys keyword)
                    (update-vals parse-long))))]))

(comment
  (map parse-line (str/split-lines test-data)))

(comment
  (def limits {:red 12
               :green 13
               :blue 14})
  (every? (fn [[n color]] (<= n (limits color))) '([3 :green] [15 :blue] [14 :red]))
  (every? (fn [[n color]] (<= n (limits color))) '([6 :red] [1 :blue] [3 :green]))
  (every? (partial every? (fn [[n color]] (<= n (limits color))))
          '(([6 :red] [1 :blue] [3 :green])
            ([2 :blue] [1 :red] [2 :green])))
  (every? (partial every? (fn [[n color]] (<= n (limits color))))
          '(([1 :green] [3 :red] [6 :blue])
            ([3 :green] [6 :red])
            ([3 :green] [15 :blue] [14 :red])))
  (->> (map2 (fn [[n color]] (<= n (limits color)))
             '(([1 :green] [3 :red] [6 :blue])
               ([3 :green] [6 :red])
               ([3 :green] [15 :blue] [14 :red])))
       flatten
       (every? true?))

  (def parsed (map parse-line (str/split-lines test-data)))
  (->> parsed
       (filter #(->> %
                     second
                     (map2 (fn [[n color]] (<= n (limits color))))
                     flatten
                     (every? true?)))
       (map first)
       (reduce +)))

(defn pt1 [data]
  (let [limits {:red 12
                :green 13
                :blue 14}]
    (->> data
         (map parse-line)
         (filter #(->> %
                       second
                       (map2 (fn [[color n]] (<= n (limits color))))
                       flatten
                       (every? true?)))
         (map first)
         (reduce +))))

(comment
  (resource-lines "day2.txt" pt1)
  ;; => 2776
  )

(comment
  (def game '(([3 :blue] [4 :red])
              ([1 :red] [2 :green] [6 :blue])
              ([2 :green])))
  
  (->> game
       (mapcat identity)
       (reduce (fn [maxs [n color]]
                 (cond-> maxs
                   (> n (maxs color)) (assoc color n)))
               {:red 0, :green 0, :blue 0}))
  )

(defn pt2 [data]
  (->> data
       (map parse-line)
       (map second)  ;No need for game ID in pt2
       (map #(apply merge-with max %))
       (map #(reduce * (vals %)))
       (reduce +)))

(comment
  (->> test-data
       str/split-lines
       pt2)
  (resource-lines "day2.txt" pt2)
  ;; => 68638

  )
