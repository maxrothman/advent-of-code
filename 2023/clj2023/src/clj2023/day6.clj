(ns clj2023.day6 
  (:require [clj2023.util :refer [map2 resource-lines]]
            [clojure.math :refer [ceil floor sqrt]]
            [clojure.string :as str]))

  ;; d = v*t
  ;; t = race - hold
  ;; v = hold
  ;; d = hold * (race - hold) 
  ;; hold = 1/2(r ± √(r^2 - 4d))
  ;; (r -) solution is (always?) smaller
  ;; win when: d > d_race 
  
(defn parse1 [lines]
  (->> (map #(re-seq #"\d+" %) lines)
       (map2 parse-long)
       (apply map vector)  ;transpose
       (map #(zipmap [:time :dist] %))))

(defn parse2 [lines]
  (->> (map #(re-seq #"\d+" %) lines)
       (map str/join)
       (map parse-long)
       (zipmap [:time :dist])
       vector))

(defn sol [lines]
  (->> (map (fn [{:keys [time dist]}]
              (let [smaller (* 1/2 (- time (sqrt (- (* time time) (* 4 dist)))))
                    larger  (* 1/2 (+ time (sqrt (- (* time time) (* 4 dist)))))]
                (inc (- (dec (ceil larger)) (inc (floor smaller))))))
            lines)
       (reduce *)))

(comment
  (resource-lines "day6.txt" (comp sol parse1))
  (resource-lines "day6.txt" (comp sol parse2))
  )