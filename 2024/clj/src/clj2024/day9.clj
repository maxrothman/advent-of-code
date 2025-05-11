(ns clj2024.day9
  (:require [medley.core :refer [indexed find-first map-entry]]
            [clojure.string :as str]))

(def test-raw
  "2333133121414131402")

(defn parse [raw]
  (->> raw
       (map (comp parse-long str))
       (partition 2 2 (repeat 0))
       (map-indexed #(into [%1] %2))))
;; id used free 

(defn expand [[id used free]]
  (concat (repeat used id)
          (repeat free nil)))

(defn to-indexed [parsed]
  (->> parsed (mapcat expand) indexed (into (sorted-map))))

(defn starting-after
  "m: map, k: starting key, fwd: bool (forward/backwards)
   Return a seq of map values in sorted order starting after k"
  [m k fwd]
  (let [[k' v] (first (if fwd (subseq m > k) (rsubseq m < k)))]
    (if (some? k')
      (lazy-seq (cons [k' v] (starting-after m k' fwd)))
      v)))

(defn step [[m fwd bkwd]]
  (let [[to-idx _] (find-first (comp nil? second) (starting-after m fwd true))
        [from-idx move] (find-first (comp some? second) (starting-after m bkwd false))]
    (if (< to-idx from-idx)
      [(-> m (assoc to-idx move) (assoc from-idx nil))
       to-idx from-idx]
      m)))

(defn pt1 [raw]
  (let [indexed (to-indexed (parse raw))]
    (->> (iterate step [indexed -1 (count indexed)])
         (drop-while vector?)
         first
         (take-while (comp some? second))
         (map #(apply * %))
         (apply +))))

(comment
  (pt1 test-raw)
  (pt1 (str/trim-newline (slurp "resources/day9.txt")))
  (let [expanded (->> test-raw parse (mapcat expand) indexed (into (sorted-map)))]
    (->> (iterate step [expanded -1 (count expanded)])
         (drop-while vector?)
         first
         (take-while (comp some? second))
         (map #(apply * %))
         (apply +)))

  (starting-after (sorted-map 0 :a 1 :b 2 :c 3 :d)
                  4 false)
  (first (rsubseq (sorted-map 0 :a 1 :b 2 :c 3 :d)
                  > 2)))

(defn to-ivls
  "Returns a map of {idx {:len, :v}}]
   idx: index at which the ivl starts
   len: length of interval
   v: file id at the interval"
  [parsed]
  (->> parsed
       (mapcat (fn [[id used free]]
                 [{:len used, :v id} {:len free, :v nil}]))
       (filter (comp pos? :len))  ;remove empty ivls
       (reduce (fn [[idx acc] v]
                 [(+ idx (:len v)) (conj acc [idx v])])
               [0 []])
       second
       (into (sorted-map))))

(defn adjust-ivl [[to-idx to] [from-idx from]]
  (let [len' (- (:len to) (:len from))]
    (->> [[to-idx from]
          [(+ to-idx (:len from))
           {:len len', :v nil}]]
         (filter (comp pos? :len second)))))

(defn step [ivls id]
  (let [[idx move] (find-first #(= id (:v (val %))) ivls)]
    (if-let [to (find-first #(and (<= (:len move) (:len (val %)))
                                  (nil? (:v (val %))))
                            (subseq ivls < idx))]
      (-> (into ivls (adjust-ivl to [idx move]))
          (assoc-in [idx :v] nil))
      ivls)))

(defn expand-chsum [[idx {:keys [len v]}]]
  (->> (range idx (+ idx len))
       (map #(* % v))))

(defn pt2 [raw]
  (let [ivls (->> raw parse to-ivls)
        ids (->> ivls vals (map :v) (filter some?) (into (sorted-set-by >)))]
    (->> (reduce step ivls ids)
         (filter (comp some? :v val))
         (mapcat expand-chsum)
         (apply +))))

(defn dbg [ivls]
  (->> (vals ivls)
       (mapcat (fn [{:keys [len v]}]
                 (repeat len (if (some? v) (str v) "."))))
       (apply str)))

(comment
  (expand-chsum [2 {:len 2, :v 9}])
  (ivl-assoc [0 {:len 4 :v nil}]
             [20 {:len 4 :v 9}])

  (pt2 (str/trim-newline (slurp "resources/day9.txt")))
  (pt2 "1313165")

  (let [ivls (->> "1313165" parse to-ivls)
        ids (->> ivls vals (map :v) (filter some?) (into (sorted-set-by >)))]
    (->> (reductions step ivls ids)
         (drop 1)
         first
         #_(map dbg)))
  (dbg (step ivls 1))
  (dbg ivls) 
  )

;; 0  0  .  .  .  1  1  1  .  .  .  2  .  .  .  3  3  3  .  4  4  .  5  5  5  5  .  6  6  6  6  .  7  7  7  .  8  8  8  8  9  9  
;; 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41