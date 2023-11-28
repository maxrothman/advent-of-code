(ns advent-of-code.day13
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]))

(defn reshape
  "Create a new sparse matrix, seeding with a default value"
  [shape val]
  (with-meta {} {:default val, :shape shape}))

(defn shape [m] (-> m meta :shape))

(defn in-bounds? [m coord]
  (every? identity
          (map #(> %1 %2)
               (shape m) coord)))

(defn mget [m coord]
  (assert (in-bounds? m coord) "Out of bounds!")
  (get m coord (:default (meta m))))

(defn mset [m coord v]
  (assert (in-bounds? m coord) "Out of bounds!")
  (assoc m coord v))

(defn rank-in-bounds? [m rank]
  (< -1 rank (count (shape m))))

(defn msplit [m rank val]
  (assert (rank-in-bounds? m rank) "Rank out of bounds!")
  (assert (in-bounds? m (assoc [0 0] rank val)) "Split value out of bounds!")
  (reduce-kv (fn [[left right] k v]
               (case (compare (get k rank) val)
                 -1 [(mset left k v) right]
                 0  [left right]
                 1  [left (mset right
                                (update k rank - val 1)
                                v)]))
             [(reshape (assoc (shape m) rank val) 0)
              (reshape (update (shape m) rank - val 1) 0)]
             m))

(defn mreverse [m rank]
  (assert (rank-in-bounds? m rank) "Rank out of bounds!")
  (let [shape-in-dim (get (shape m) rank)]
    (with-meta
      (util/map-keys #(update %
                              rank
                              (partial - shape-in-dim 1))
                     m)
      (meta m))))

(comment
  (-> (reshape [10 10] 0)
      (#(reduce (partial apply mset) % [[[0 0] 1] [[0 4] 1] [[7 2] 1] [[2 9] 1]]))
      (mreverse 1)
      mprint
      println))

(defn mor [m1 m2]
  (assert (= (shape m1) (shape m2)) "Matrices must have same shape!")
  (assert (apply = (map (comp :default meta) [m1 m2])) "Matrices must have same type!")
  (merge-with #(first [%1 %2]) m1 m2))

(defn mpad [m rank n]
  (with-meta
    (util/map-keys #(update % rank + n)
                   m)
    (update-in (meta m)
               [:shape rank]
               + n)))

(comment
  (-> (reshape [10 10] 0)
      (#(reduce (partial apply mset) % [[[0 0] 1] [[0 4] 1] [[7 2] 1] [[2 9] 1]]))
      (mpad 1 3)
      mprint
      println))

(defn pad-to-match [m1 m2]
  (let [[m1-xrank m1-yrank] (shape m1)
        [m2-xrank m2-yrank] (shape m2)]
    [(cond-> m1
       (< m1-xrank m2-xrank) (mpad 0 (- m2-xrank m1-xrank))
       (< m1-yrank m2-yrank) (mpad 1 (- m2-yrank m1-yrank)))
     (cond-> m2
       (< m2-xrank m1-xrank) (mpad 0 (- m1-xrank m2-xrank))
       (< m2-yrank m1-yrank) (mpad 1 (- m1-yrank m2-yrank)))]))

(comment
  (let [m1 (reduce #(mset %1 %2 1)
                   (reshape [5 2] 0)
                   [[0 0] [1 1] [0 1] [4 1]])
        m2 (reduce #(mset %1 %2 1)
                   (reshape [2 2] 0)
                   [[0 0] [1 1] [0 1] [1 0]])]
    (->> (pad-to-match m1 m2)
         (map (comp println println mprint))
         dorun))
  )

(defn parse-input [data]
  (let [[coords folds] (str/split data #"\n\n")
        coords (->> coords
                    str/split-lines
                    (map #(str/split % #","))
                    (map (partial mapv #(Integer/parseInt %))))
        folds (->> folds
                          str/split-lines
                          (map #(re-find #"fold along (.)=(.*)" %))
                          (map (partial drop 1))
                          (map #(vector
                                 (case (first %)
                                   "x" 0
                                   "y" 1)
                                 (Integer/parseInt (second %)))))
        x-shape (->> coords
                     (map first)
                     (reduce max)
                     inc)
        y-shape (->> coords
                     (map second)
                     (reduce max)
                     inc)]
    [(reduce #(mset %1 %2 1)
             (reshape [x-shape y-shape] 0)
             coords)
     folds]))

(defn mprint [m]
  (let [[xmax ymax] (shape m)]
    (str/join "\n"
              (for [y (range ymax)]
                (str/join ""
                          (for [x (range xmax)]
                            (if (= 1 (mget m [x y]))
                              "#"
                              ".")))))))

(def test-input
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn trace [x]
  (->> x (map shape) (prn "shapes: "))
  x)

(defn solution [input]
  (let [[m folds] (parse-input input)]
    (reductions (fn [m [rank val]]
                  (as-> m x
                    (msplit x rank val)
                    (update x 1 mreverse rank)
                    (apply pad-to-match x)
                    (apply mor x)))
                m
                folds)))

(comment
  (-> (solution test-input)
      mprint
      println)
;; #####
;; #...#
;; #...#
;; #...#
;; #####
;; .....
;; .....
  )

;; Part 1
(comment
  (-> (slurp "resources/day13/input.txt")
      solution
      second
      count)
  )
;; => 837

;; Part 2
(comment
  (-> (slurp "resources/day13/input.txt")
      solution
      last
      mprint
      println)
;; ####.###..####..##..#..#..##..#..#.#..#.
;; #....#..#....#.#..#.#.#..#..#.#..#.#..#.
;; ###..#..#...#..#....##...#....####.#..#.
;; #....###...#...#.##.#.#..#....#..#.#..#.
;; #....#....#....#..#.#.#..#..#.#..#.#..#.
;; ####.#....####..###.#..#..##..#..#..##..
  (->> (slurp "resources/day13/input.txt")
       solution
       (map mprint)
       (str/join "\n\n")
       (spit "resources/day13/output.txt"))
  )