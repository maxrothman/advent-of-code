(ns advent-of-code.day11
  (:require [clojure.core.matrix :as mat]
            [clojure.string :as str]))

(def test1
  [[5 4 8 3 1 4 3 2 2 3]
   [2 7 4 5 8 5 4 7 1 1]
   [5 2 6 4 5 5 6 1 7 3]
   [6 1 4 1 3 3 6 1 4 6]
   [6 3 5 7 3 8 5 4 7 8]
   [4 1 6 7 5 2 4 6 4 5]
   [2 1 7 6 8 4 1 7 2 1]
   [6 8 8 2 8 8 1 1 3 4]
   [4 8 4 6 8 4 8 5 5 4]
   [5 2 8 3 7 5 1 5 2 6]])

(def test1-expected
  [[[5 4 8 3 1 4 3 2 2 3]
    [2 7 4 5 8 5 4 7 1 1]
    [5 2 6 4 5 5 6 1 7 3]
    [6 1 4 1 3 3 6 1 4 6]
    [6 3 5 7 3 8 5 4 7 8]
    [4 1 6 7 5 2 4 6 4 5]
    [2 1 7 6 8 4 1 7 2 1]
    [6 8 8 2 8 8 1 1 3 4]
    [4 8 4 6 8 4 8 5 5 4]
    [5 2 8 3 7 5 1 5 2 6]]

   [[6 5 9 4 2 5 4 3 3 4]
    [3 8 5 6 9 6 5 8 2 2]
    [6 3 7 5 6 6 7 2 8 4]
    [7 2 5 2 4 4 7 2 5 7]
    [7 4 6 8 4 9 6 5 8 9]
    [5 2 7 8 6 3 5 7 5 6]
    [3 2 8 7 9 5 2 8 3 2]
    [7 9 9 3 9 9 2 2 4 5]
    [5 9 5 7 9 5 9 6 6 5]
    [6 3 9 4 8 6 2 6 3 7]]
   [[8 8 0 7 4 7 6 5 5 5]
    [5 0 8 9 0 8 7 0 5 4]
    [8 5 9 7 8 8 9 6 0 8]
    [8 4 8 5 7 6 9 6 0 0]
    [8 7 0 0 9 0 8 8 0 0]
    [6 6 0 0 0 8 8 9 8 9]
    [6 8 0 0 0 0 5 9 4 3]
    [0 0 0 0 0 0 7 4 5 6]
    [9 0 0 0 0 0 0 8 7 6]
    [8 7 0 0 0 0 6 8 4 8]]

   [[0 0 5 0 9 0 0 8 6 6]
    [8 5 0 0 8 0 0 5 7 5]
    [9 9 0 0 0 0 0 0 3 9]
    [9 7 0 0 0 0 0 0 4 1]
    [9 9 3 5 0 8 0 0 6 3]
    [7 7 1 2 3 0 0 0 0 0]
    [7 9 1 1 2 5 0 0 0 9]
    [2 2 1 1 1 3 0 0 0 0]
    [0 4 2 1 1 2 5 0 0 0]
    [0 0 2 1 1 1 9 0 0 0]]

   [[2 2 6 3 0 3 1 9 7 7]
    [0 9 2 3 0 3 1 6 9 7]
    [0 0 3 2 2 2 1 1 5 0]
    [0 0 4 1 1 1 1 1 6 3]
    [0 0 7 6 1 9 1 1 7 4]
    [0 0 5 3 4 1 1 1 2 2]
    [0 0 4 2 3 6 1 1 2 0]
    [5 5 3 2 2 4 1 1 2 2]
    [1 5 3 2 2 4 7 2 1 1]
    [1 1 3 2 2 3 0 2 1 1]]

   [[4 4 8 4 1 4 4 0 0 0]
    [2 0 4 4 1 4 4 0 0 0]
    [2 2 5 3 3 3 3 4 9 3]
    [1 1 5 2 3 3 3 2 7 4]
    [1 1 8 7 3 0 3 2 8 5]
    [1 1 6 4 6 3 3 2 3 3]
    [1 1 5 3 4 7 2 2 3 1]
    [6 6 4 3 3 5 2 2 3 3]
    [2 6 4 3 3 5 8 3 2 2]
    [2 2 4 3 3 4 1 3 2 2]]

   [[5 5 9 5 2 5 5 1 1 1]
    [3 1 5 5 2 5 5 2 2 2]
    [3 3 6 4 4 4 4 6 0 5]
    [2 2 6 3 4 4 4 4 9 6]
    [2 2 9 8 4 1 4 3 9 6]
    [2 2 7 5 7 4 4 3 4 4]
    [2 2 6 4 5 8 3 3 4 2]
    [7 7 5 4 4 6 3 3 4 4]
    [3 7 5 4 4 6 9 4 3 3]
    [3 3 5 4 4 5 2 4 3 3]]

   [[6 7 0 7 3 6 6 2 2 2]
    [4 3 7 7 3 6 6 3 3 3]
    [4 4 7 5 5 5 5 8 2 7]
    [3 4 9 6 6 5 5 7 0 9]
    [3 5 0 0 6 2 5 6 0 9]
    [3 5 0 9 9 5 5 5 6 6]
    [3 4 8 6 6 9 4 4 5 3]
    [8 8 6 5 5 8 5 5 5 5]
    [4 8 6 5 5 8 0 6 4 4]
    [4 4 6 5 5 7 4 6 4 4]]

   [[7 8 1 8 4 7 7 3 3 3]
    [5 4 8 8 4 7 7 4 4 4]
    [5 6 9 7 6 6 6 9 4 9]
    [4 6 0 8 7 6 6 8 3 0]
    [4 7 3 4 9 4 6 7 3 0]
    [4 7 4 0 0 9 7 6 8 8]
    [6 9 0 0 0 0 7 5 6 4]
    [0 0 0 0 0 0 9 6 6 6]
    [8 0 0 0 0 0 4 7 5 5]
    [6 8 0 0 0 0 7 7 5 5]]

   [[9 0 6 0 0 0 0 6 4 4]
    [7 8 0 0 0 0 0 9 7 6]
    [6 9 0 0 0 0 0 0 8 0]
    [5 8 4 0 0 0 0 0 8 2]
    [5 8 5 8 0 0 0 0 9 3]
    [6 9 6 2 4 0 0 0 0 0]
    [8 0 2 1 2 5 0 0 0 9]
    [2 2 2 1 1 3 0 0 0 9]
    [9 1 1 1 1 2 8 0 9 7]
    [7 9 1 1 1 1 9 9 7 6]]

   [[0 4 8 1 1 1 2 9 7 6]
    [0 0 3 1 1 1 2 0 0 9]
    [0 0 4 1 1 1 2 5 0 4]
    [0 0 8 1 1 1 1 4 0 6]
    [0 0 9 9 1 1 1 3 0 6]
    [0 0 9 3 5 1 1 2 3 3]
    [0 4 4 2 3 6 1 1 3 0]
    [5 5 3 2 2 5 2 3 5 0]
    [0 5 3 2 2 5 0 6 0 0]
    [0 0 3 2 2 4 0 0 0 0]]])

(def test2
  [[1 1 1 1 1]
   [1 9 9 9 1]
   [1 9 1 9 1]
   [1 9 9 9 1]
   [1 1 1 1 1]])

(defn neighbors [[y x]]
  (for [xx [-1 0 1]
        yy [-1 0 1]
        :when (not= [yy xx] [0 0])]
    [(+ y yy) (+ x xx)]))

(defn map-indexed-topref [f mat]
  (mat/emap-indexed #(f %1 %2 mat) mat))

(defn flash? [x] (< 9 x))

(defmacro fork
  "(f (g x) (h x))
   Only a macro because some common fns like 'or' are macros"
  [f g h x]
  `(~f (~g ~x) (~h ~x)))
(comment
  (fork or flash? zero? 10)
  ;; => true
  )

(defn until [pred f x]
  (->> (iterate f x)
       (drop-while (complement pred))
       first))
(comment
  (until #(= 0 (mod % 10)) dec 19)
  ;; => 10
  (until #(= 0 (mod % 10)) dec 27)
  ;; => 20
  )

(defn add-neighbors [idx val mat]
  (if (fork or flash? zero? val)
    0
    (->> (neighbors idx)
         (map #(get-in mat % -1))
         (filter flash?)
         count
         (+ val))))

(comment
  (add-neighbors [0 1] 4 (mat/emap inc test1)))

(defn step [data]
  (->> data
       (mat/emap inc)
       (#(vector nil %))
       (until (partial apply =)
              #(vector (second %)
                       (map-indexed-topref add-neighbors (second %))))
       first))

(defn run [data steps]
  (->> data
       (iterate step)
       (take (inc steps))))

(comment
  (let [res (run data 10)]
    (and (= res test1-expected)
         (= 204 (->> res
                     mat/eseq
                     (filter zero?)
                     count))))
  ;; => true

  (let [res (run data 100)]
    (= 1656 (->> res
                 mat/eseq
                 (filter zero?)
                 count)))
  ;; => true
  )

(comment

  (def data
    (->> "resources/day11/input.txt"
         slurp
         (#(str/split % #"\n"))
         (map #(str/split % #""))
         (mat/emap #(Integer/parseInt %))))

  ;; Part 1
  (->> (run data 100)
       mat/eseq
       (filter zero?)
       count)
  ;; => 1659

  ;; Part 2
  (->> (run data 400)
       (take-while #(not (every? zero? (mat/eseq %))))
       count)
  ;; => 227
)