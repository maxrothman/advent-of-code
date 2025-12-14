(ns day1
  (:require
   [clojure.string :as str]
   [util :refer [resource-lines]]))

(def tst
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defn forkf [f g h]
  (fn [x] (f (g x) (h x))))

(defn fork [f g h x]
  (f (g x) (h x)))

(defn parse [input-lines]
  (->> (map (forkf vector first (comp parse-long #(subs % 1)))
            input-lines)
       (map (fn [[dir v]] (case dir
                            \L (- v)
                            \R v)))
       doall))

(def start 50)
(def modulus 100)

(defn step [a x] (mod (+ a x) modulus))

(defn pt1 [input]
  (->> input
       (reductions step start)
       (filter zero?)
       count))

(defn step2 [[a _] x]
  ;; Flip the start point about the modulus (see below). 0 should stay at 0.
  (let [flipped (mod (- modulus a) modulus)]
    [(mod (+ a x) modulus)
     (if (pos? x)
       (quot (+ a x) modulus)
       ;; Instead of moving left, flip the start point about the modulus, move right, and see if we
       ;; crossed 100
       (quot (+ flipped (abs x)) modulus))]))

(defn pt2 [input]
  (->> input
       (reductions step2 [start 0])
       (map second)
       (apply +)))

(comment
  (pt1 (resource-lines "day1.txt" parse))
  (pt2 (resource-lines "day1.txt" parse) #_(parse (str/split-lines tst)))
  
  (pt2 [1000 -1000 -50 1 -1 -1 1 100 1])
  (step2 [0] -1)
  
  (mod 112 100)
  (mod -250 100)
  (quot -250 100)
  [20 -40]
  (mod (+ 20 -45) 100) ;=>75
  
  (quot (+ 75 45) 100)
  (quot (+ 95 (abs -10)) 100)
  ;; 0 -> 100
  ;; 95 -> 95
  (+ 1 (mod (+ 2 99) 100))

  (+ 10 (- 100 20))
  )