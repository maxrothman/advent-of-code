(ns clj2024.day1
  (:require
   [clj2024.util :refer [spy resource-lines]]
   [clojure.string :as str]))

(def test-raw
  "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse [lines]
  (->> lines
       (map #(->> (str/split % #" +") (map parse-long)))
       (apply map vector)))

(comment
  (parse (str/split-lines test-raw)))

(defn pt1 [l1 l2]
  (->> (map #(abs (- %1 %2))
            (sort l1) (sort l2))
       (apply +)))

(comment
  (apply pt1 (parse (str/split-lines test-raw)))

  (resource-lines "day1.txt" #(apply pt1 (parse %)))
  )

(defn pt2 [l1 l2]
  (let [freqs (frequencies l2)]
    (->> (map #(* % (get freqs % 0)) l1)
         (apply +))))

(comment
  (apply pt2 (parse (str/split-lines test-raw)))
  (resource-lines "day1.txt" #(apply pt2 (parse %))))
  )