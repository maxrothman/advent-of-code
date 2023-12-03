(ns clj2023.day3
  (:require [clj2023.util :refer [resource-lines]]
            [clojure.string :as str]
            [medley.core :refer [map-vals]]))

(def test-data
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn re-find* [re s]
  (let [m (re-matcher re s)]
    (->> (repeatedly #(if (.find m) m nil))
         (take-while some?)
         (map #(hash-map :match (.group %)
                         :start (.start %)
                         :end (.end %))))))

(defn parse [lines]
  (->> lines
       (map-indexed #(vector %1 (re-find* #"\d+|[^a-z\d.]" %2)))
       (mapcat (fn [[y ms]] (map #(assoc % :y y) ms)))
       (mapcat (fn [{:keys [y start end] :as match}]
                 (map #(vector [% y] match) (range start end))))

       (into {})
       (map-vals #(cond-> % (re-matches #"\d+" (:match %)) (update :match parse-long)))))

(defn adjacent [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]       ,,,         [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn pt1 [lines]
  (let [parsed (parse lines)]
    (->> parsed
         (filter (comp string? :match val))    ;only symbols
         (map key)                             ;keep coords, ditch vals
         (mapcat adjacent)
         (keep #(get parsed %))
         set                                   ;dedupe
         (map :match)
         (reduce +))))

(comment
  (def lines (str/split-lines test-data))
  (map #(re-find* #"\d+|[^a-z\d.]" %) (str/split-lines test-data))

  {[0 0] 467
   [1 0] 467
   [2 0] 467
   [3 1] "*"
   ; ...
   }
  (resource-lines "day3.txt" #(->> % (mapcat (partial re-seq #"\d+")) count))
  ;; => 1216
  (resource-lines "day3.txt" #(->> % (mapcat (partial re-seq #"\d+")) set count))
  ;; => 716
  ;; There are multiples of the same number adjacent to a single symbol

  (resource-lines "day3.txt" pt1)
  ;; => 544664

  )

(defn pt2 [lines]
  (let [parsed (parse lines)]
    (->> parsed
         (filter (comp #(= "*" %) :match val))
         (map key)                       ;get coords
         (map (comp set                  ;adjacent parts
                    #(filter some? %)
                    #(map get (repeat parsed) %)
                    adjacent))
         (filter #(= 2 (count %)))
         (map #(reduce * (map :match %)))
         (reduce +))))

(comment
  (pt2 (str/split-lines test-data))
  ;; => 467835

  (resource-lines "day3.txt" pt2)
  ;; => 84495585

  )