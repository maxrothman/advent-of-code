(ns clj2024.day2 
  (:require
   [clj2024.util :refer [resource-lines]]
   [clojure.string :as str]))

(def test-raw
  (str/split-lines
   "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))

(defn parse [l]
  (->> (re-seq #"\d+" l)
       (map parse-long)))

(defn safe? [l]
  (and (or (apply < l)
           (apply > l))
       (every? (fn [[a b]] (<= 1 (abs (- a b)) 3))
               (partition 2 1 l))))

(comment
  (->> test-raw
       (filter (comp safe? parse))
       count)
  
  (resource-lines "day2.txt"
                  #(->> %
                        (filter (comp safe? parse))
                        count))
  )

(defn without-any-1 [l]
  (loop [[cur & rst] l, before [], accum '()]
    (if cur
      (recur rst (conj before cur) (conj accum (concat before rst)))
      accum)))

(comment
  (without-any-1 [1 2 3 4 5])

  (->> test-raw
       (map (comp without-any-1 parse))
       (filter #(some safe? %))
       count)
  (resource-lines "day2.txt"
                  (fn [ls]
                    (->> ls
                         (map (comp without-any-1 parse))
                         (filter #(some safe? %))
                         count)))
  )