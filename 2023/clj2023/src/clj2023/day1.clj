(ns clj2023.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [helins.interval.set :as iset]
            [clojure.reflect :as reflect]))

(defn parse []
  (->> (io/resource "day1.txt")
       io/reader
       line-seq))

(defn first-digit [s]
  (->> s
       (re-seq #"[^\d]*(\d)")
       first second))

(defn last-digit [s]
  (->> s
       (re-seq #".*(\d)[^\d]*$")
       first second))

(defn pt1 []
  (->> (parse)
       (map (juxt first-digit last-digit))
       (map str/join)
       (map parse-long)
       (reduce +)))

(comment
  (pt1)
  ;; => 55477
  )

(def nums
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def digit-re
  (str "\\d|" (str/join "|" (keys nums))))

(def first-re
  (re-pattern (str "^(?:(?!" digit-re ").)*(" digit-re ")")))

(def last-re
  (re-pattern (str ".*(" digit-re ")(?:(?!" digit-re ").)*$")))

(defn match [re s]
  (->> s
       (re-seq re)
       first second))

(defn num->digit [s]
  (if (re-matches #"\d" s)
    s
    (nums s)))

(defn pt2 []
  (->> (parse)
       (map (juxt (partial match first-re) (partial match last-re)))
       (map #(mapv num->digit %))
       (map str/join)
       (map parse-long)
       (reduce +)))

(comment
  (pt2)
  ;; => 54431
  )