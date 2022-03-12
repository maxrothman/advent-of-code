(ns advent-of-code.day14
  (:require [clojure.string :as str]
            [advent-of-code.util :as util]))

(def test-input
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn parse [raw]
  (let [[orig inserts] (str/split raw #"\n\n")
        inserts (->> inserts
                     str/split-lines
                     (map #(re-find #"([^ ])([^ ]) -> ([^ ])" %))
                     (map rest)
                     (map #(map first %)) ;Single-character strs -> chars
                     (map (fn [[a b c]] [[a b] c]))
                     (into {}))]
    [orig inserts]))

(defn step [inserts orig]
  (->> (partition 2 1 orig)
       (reduce (fn [s window]
                 (if-let [insert (inserts window)]
                   (conj s insert (second window))
                   (apply conj s (second window))))
               [(first orig)])
       str/join))

(def counter (atom 0))
(defn count-hits []
  (swap! counter inc)
  (prn (new java.util.Date) @counter))

(comment
  ;; Part 1
  (let [[s inserts] (parse test-input)]
    (->> (iterate (partial step inserts) s)
         (take 5)
         (= ["NNCB"
             "NCNBCHB"
             "NBCCNBBBCBHCB"
             "NBBBCNCCNBBNBNBBCHBHHBCHB"
             "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"])
         assert)
    (->> (iterate (partial step inserts) s)
         (#(nth % 10))
         frequencies
         (sort-by second)
         (util/fork vector last first)
         (map second)
         (apply -)
         (= 1588)
         assert))

  (let [[s inserts] (parse (slurp "resources/day14/input.txt"))]
    (->> (iterate (partial step inserts) s)
         (#(nth % 10))
         frequencies
         (sort-by second)
         (util/fork vector last first)
         (map second)
         (apply -)))
  ;; => 3697

  ;; Part 2
  (let [[s inserts] (parse (slurp "resources/day14/input.txt"))]
    (->> (iterate #(do (count-hits) (step inserts %)) s)
         (#(nth % 40))
         frequencies
         (sort-by second)
         (util/fork vector last first)
         (map second)
         (apply -))))
  ;; Will take a total of almost 6184 hours to complete, not tenable
  ;; Parse input str into pairs, map pairs to counts


;; Part 2 take 2
(defn str->pairs [s]
  (->> s
       ;; Pad the last pair with nil so we can track the contribution of the last letter to the
       ;; total count per letter
       (partition 2 1 (repeat nil))
       frequencies))

(defn some-int [x]
  (if (some? x) x 0))

(defn step2 [inserts pairs]
  (reduce-kv (fn [m [c1 c2] v]
               (if-let [new-c (inserts [c1 c2])]
                 (-> m
                     (update [c1 new-c]
                             (comp (partial + v) some-int))
                     (update [new-c c2]
                             (comp (partial + v) some-int)))
                 (assoc m [c1 c2] v)))
             {}
             pairs))

(defn flatten-pairs [pair-freqs]
  (reduce (fn [m [[k1 _] v]]
            (update m k1 #(+ v (some-int %))))
          {}
          pair-freqs))

(comment
  ;; str->pairs and step work
  (let [[s inserts] (parse test-input)]
    (->> s
         str->pairs
         (iterate (partial step2 inserts))
         (take 5)
         (= (map str->pairs ["NNCB"
                             "NCNBCHB"
                             "NBCCNBBBCBHCB"
                             "NBBBCNCCNBBNBNBBCHBHHBCHB"
                             "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"]))
         assert))

  ;; flatten-pairs works
  (let [[s inserts] (parse test-input)]
    (->> s
         str->pairs
         (iterate (partial step2 inserts))
         (take 5)
         (map flatten-pairs)
         (= (map frequencies
                 ["NNCB"
                  "NCNBCHB"
                  "NBCCNBBBCBHCB"
                  "NBBBCNCCNBBNBNBBCHBHHBCHB"
                  "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"]))
         assert))

  ;; After 5th iteration, length = 97
  (let [[s inserts] (parse test-input)]
    (->> s
         str->pairs
         (iterate (partial step2 inserts))
         (#(nth % 5))
         flatten-pairs
         (map second)
         (reduce +)
         (= 97)
         assert))

  ;; Correctly calculates test result
  (let [[s inserts] (parse test-input)]
    (->> s
         str->pairs
         (iterate (partial step2 inserts))
         (#(nth % 10))
         flatten-pairs
         (sort-by second)
         (util/fork vector last first)
         (map second)
         (apply -)
         (= 1588)
         assert))
  
  ;; Aaaand the grand reveal
  (let [[s inserts] (parse (slurp "resources/day14/input.txt"))]
    (->> s
         str->pairs
         (iterate (partial step2 inserts))
         (#(nth % 40))
         flatten-pairs
         (sort-by second)
         (util/fork vector last first)
         (map second)
         (apply -)))
  ;; => 4371307836157

  )