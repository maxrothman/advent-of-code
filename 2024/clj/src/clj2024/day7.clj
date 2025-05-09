(ns clj2024.day7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [clj2024.util :as util]))

(def test-raw "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-line [l]
  (let [[tst & nums] (map parse-long (re-seq #"\d+" l))]
    [tst nums]))

(defn valid? [tst [n-init & nums] ops]
  (loop [[n & nrst] nums
         [op & oprst] ops
         acc n-init]
    (let [result (op acc n)]
      (cond
        (> result tst) false
        (and (empty? nrst) (= result tst)) true
        (empty? nrst) false
        :else (recur nrst oprst result)))))

(defn sln [data ops]
  (->> (for [[tst nums] data
             :let [ops (comb/selections ops (dec (count nums)))]
             :when (some #(valid? tst nums %) ops)]
         tst) 
       (apply +)))

(comment
  (sln (map parse-line (str/split-lines test-raw))
       [(comp parse-long str) * +])
  (sln (map parse-line (str/split-lines "4: 2 2 100")) [* +])
  (util/resource-lines "day7.txt" #(sln (map parse-line %) [* +]))
  (util/resource-lines "day7.txt"
                       #(sln (map parse-line %)
                             [(comp parse-long str) * +]))
  )
