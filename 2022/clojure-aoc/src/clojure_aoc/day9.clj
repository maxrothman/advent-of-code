(ns clojure-aoc.day9
  (:require [clojure-aoc.util :refer [as->> spy]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn visualize [symbols points]
  (let [xmax (inc (apply max (map first points)))
        ymax (inc (apply max (map second points)))]
    (->> (vec (repeat ymax (vec (repeat xmax "."))))
         (as->> grid (reduce #(apply assoc-in %1 %2)
                             grid
                             (map vector (map reverse points) symbols)))
         (map str/join)
         (str/join "\n"))))
(comment
  (println (visualize "HT" [[1 2] [2 2]]))
  (println (visualize (repeat 5 \#) [[1 1] [2 2] [3 3] [4 5] [7 7]])))

(def input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def parse-dir
  {"U" [0 -1]
   "D" [0 1]
   "R" [1 0]
   "L" [-1 0]})

(defn parse [input]
  (->> input
       (map #(str/split % #" "))
       (map (juxt (comp parse-long last) (comp parse-dir first)))
       (mapcat #(apply repeat %))))
(comment
  (parse (str/split-lines input))
  )

(defn step [[hpos tpos] dir] 
  (let [new-hpos (map + hpos dir)]
    [new-hpos
     (if (->> (map - new-hpos tpos)
              (map abs)
              (some #(< 1 %)))
       (map - new-hpos dir)
       tpos)]))
(comment
  (step '(4 0) [5 0] '([-1 0] [-1 0])) 
  )

(defn simulate [hpos tpos actions]
  (reductions step [hpos tpos] actions))
(comment
  (->> (simulate [5 0] [5 0] [[-1 0] [-1 0] [-1 0]])
       (map #(take 2 %))
       (map visualize)
       (run! #(do (println %) (println))))

  ;; Part 1
  (with-open [f (io/reader (io/resource "day9.txt"))]
    (->> (parse (line-seq f))
         (simulate [0 0] [0 0])
         (map second)
         set
         count))
  ;; => 6023
  
  )

;; Part 2

(defn follow [dir h t]
  (if (->> (map - h t)
           (map abs)
           (some #(< 1 %)))
    
    t))

(defn step2 [[h & followers] dir]
  (reductions (partial follow dir) (map + h dir) followers))

(defn simulate2 [knots actions]
  (reductions step knots actions))

(comment
  (reductions step2 [[3 0] [2 0] [1 0]] [[0 1] [0 1]])
  )
;; ......
;; ......
;; ....H.
;; ...21.
;; 43....
(comment
  (println (visualize "HT" [[0 3] [1 1]]))
  (map - [0 3] [1 1])
  
;; => (-1 2)
;; Want tail to be [0 2]
  (map + [1 1] [-1 1])
  
;; => (0 2)
  
  (defn sign [x]
    (if (zero? x)
      0
      (/ x (abs x))))
  )

(defn follow [h t]
  (->> (map - h t)
       (map sign)
       (map + t)))
(comment
  (map ff (repeat [3 0]) [[1 1]
                          [1 0]])
  )

(defn step [[h & followers] dir]
  (reductions follow (map + h dir) followers))

(defn simulate [knots actions]
  (reductions step knots actions))

(comment
  (->> #_#_(io/resource "day9.txt")
         slurp
   input
       str/split-lines
       parse
       (simulate [[5 5] [5 5]])
       (map second)
       (visualize (repeat "#"))
       println
       #_#_
           set
         count)
  )
;; ...........
;; ......####.
;; ......#####
;; .....######
;; .........#.
;; .....#####.

;; There's something wrong with (follow ), this doesn't match their picture
