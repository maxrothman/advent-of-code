(ns clj2023.day11 
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(defn dist
  "Manhattan distance"
  [[a b]]
  (->> (map - a b)
       (map abs)
       (apply +)))

(defn ->coords [m]
  (for [[y l] (map-indexed vector m)
        [x v] (map-indexed vector l)
        :when (= \# v)]
    [x y]))

(defn transpose [m]
  (apply mapv str m))

(defn expand-1 [m]
  (mapcat #(if (re-matches #"\.+" %) [% %] [%]) m))

(defn expand [m]
  (expand-1 (transpose (expand-1 m))))

(def test-data
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn pt1 [m]
  (->> (combo/combinations (->coords (expand m)) 2)
       (map dist)
       (reduce +)))

(comment
  (pt1 (str/split-lines test-data))
  (pt1 (str/split-lines (slurp (io/resource "day11.txt"))))
  )

;; Using a sorted map for quickly finding all coordinates with major>empty space, so need y to be
;; the major axis so it sorts by y first
(defn ->coords2 [m]
  (for [[y l] (map-indexed vector m)
        [x v] (map-indexed vector l)
        :when (= \# v)]
    [y x]))

(def ^:dynamic *expand-factor* nil)
(defn expand2-1 [coords m]
  (reduce (fn [cs [i l]]
            (if (re-matches #"\.+" l)
              (->> (subseq cs > [i 0])
                   (map key)
                   (reduce #(update-in %1 [%2 0] (partial + *expand-factor*)) cs))
              cs))
          coords
          (map-indexed vector m)))

(defn transpose-map [coords]
  (into (sorted-map)
        (-> coords
            (update-keys (comp vec reverse))
            (update-vals (comp vec reverse)))))

(defn expand2 [coords m]
  (let [old->new (into (sorted-map) (map vector coords coords))
        ex-ys (expand2-1 old->new m)
        ex-xs (transpose-map (expand2-1 (transpose-map ex-ys) (transpose m)))]
    (vals ex-xs)))

(defn pt2 [m]
  (->> (combo/combinations (expand2 (->coords2 m) m) 2)
       (map dist)
       (reduce +)))

(comment
  (binding [*expand-factor* 1]
    (let [m (str/split-lines test-data)]
      (= (disp (expand2 (->coords2 m) m))
         (str/join "\n" (transpose (expand m))))))
  (binding [*expand-factor* 99]
   (pt2 (str/split-lines test-data)))
  (binding [*expand-factor* (dec 1000000)]
    (pt2 (str/split-lines (slurp (io/resource "day11.txt")))))
  )

;; I think I was assuming my coordinates were [y x] and they're [x y] and that's screwing something up

(defn disp [coords]
  (let [[max-y max-x] (for [f [first second]]
                        (apply max (map f coords)))
        cs (set coords)]
    (->> (map (fn [y]
                (map #(if (cs [y %]) \# \.)
                     (range (inc max-x))))
              (range (inc max-y)))
         (map str/join)
         (str/join "\n"))))
