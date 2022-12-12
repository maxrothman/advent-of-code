(ns clojure-aoc.day11 
  (:require [clojure-aoc.util :refer [as->> descending]]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import java.lang.ProcessHandle))

(def input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse-operation [expression]
  (let [[_ op-s right-s] (str/split expression #" ")
        op ({"+" +, "*" *} op-s)]
    (fn [input] 
      (op input
          (if (= "old" right-s)
            input
            (parse-long right-s))))))

(defn parse-monkey [text]
  (let [parsers [[:id #(re-find #"\d+" %)]
                 [:items #(mapv parse-long (re-seq #"\d+" %))]
                 [:operation #(->> (re-matches #"  Operation: new = (.+)" %)
                                   second
                                   parse-operation)]
                 [:test #(parse-long (re-find #"\d+" %))]
                 [:true #(re-find #"\d+" %)]
                 [:false #(re-find #"\d+" %)]]]
    (into {} (map (fn [l [k f]] [k (f l)])
                  (str/split-lines text)
                  parsers))))

(defn parse [input]
  (-> input
      (str/split #"\n\n")
      (as-> x (map parse-monkey x))
      (as-> x (zipmap (map :id x) (map #(dissoc % :id) x)))))

(defn run-item [[monkeys id]]
  (let [m (monkeys id)
        item (first (:items m))
        worry (bigint (/ ((:operation m) item) 3))
        throw-to (if (= 0 (mod worry (:test m)))
                   (:true m)
                   (:false m))]
    [(-> monkeys
         (update-in [id :items] (comp vec rest))
         (update-in [throw-to :items] conj worry))
     id]))

(defn run-item2-try1 [[monkeys id]]
  (let [m (monkeys id)
        item (first (:items m))
        worry (bigint ((:operation m) item))
        throw-to (if (= 0 (mod worry (:test m)))
                   (:true m)
                   (:false m))]
    [(-> monkeys
         (update-in [id :items] (comp vec rest))
         (update-in [throw-to :items] conj worry))
     id]))

(defn run-monkey [item-runner monkeys id]
  (->> (update monkeys id
               (fn [m]
                 (update m :inspected
                         #(+ (or % 0)
                             (count (:items m))))))
       (as->> ms (iterate item-runner [ms id]))
       (drop-while #(seq (get-in (first %) [id :items])))
       first
       first))

(defn run-round [item-runner monkeys]
  (reduce (partial run-monkey item-runner) monkeys (sort (keys monkeys))))

(defn sol [round item-runner input]
  (->> (parse input)
       (iterate (partial run-round item-runner))
       (drop round)
       first
       vals
       (map :inspected)
       (sort descending)
       (take 2)
       (apply *)))

(comment
  (sol 20 run-item input)
  ;; => 10605
  (sol 20 run-item (slurp (io/resource "day11.txt")))
  ;; => 110264
  ;; Part 2
  (time (sol 120 run-item2-try1 (slurp (io/resource "day11.txt"))))
  ;; Too slow, scales much faster than linear
  ;; ~12s for 100, 30s for 120
  )
