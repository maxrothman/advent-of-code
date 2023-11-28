(ns advent-of-code.day17 
  (:require [advent-of-code.util :as util]
            [clojure.math :as math]))

(def start-pos [0 0])
(def target {:x [20 30]
             :y [-5 10]})

(defn iterate-x [{:keys [x-pos x-vel] :as m}]
  (assoc m
         :x-pos (+ x-pos x-vel)
         :x-vel (dec x-vel) #_(max 0 (dec x-vel))))
;; Clamping getting done in iteration, using x-vel = -1 as a break condition

(comment
  ;; Example
  (->> (iterate iterate-x {:x-pos 0 :x-vel 7})
       (drop 6)
       first)
  )

(def target {:x [282 314]
             :y [-80 -45]})

(comment
  ;; Solution
  
  ;; x: guess and check
  (->> (iterate iterate-x {:x-pos 0 :x-vel 24})
       (take 20)
       (map :x-pos)
       last)
  ;; => 285
  ;; X: 24

  )

(def start-x 24)

(defn iterate-y [{:keys [y-pos y-vel] :as m}]
  (assoc m
         :y-pos (+ y-pos y-vel)
         :y-vel (dec y-vel)))

(comment
  (->> (iterate iterate-y {:y-pos 0 :y-vel 3})
       (drop 7)
       first)
  )

(defn y-path [y-try]
  (->> (iterate iterate-y {:y-pos 0 :y-vel y-try})
       (take-while #(>= (:y-pos %) (-> target :y first)))
       (some #(<= (-> target :y first)
                  (:y-pos %)
                  (-> target :y second)))))
(def y-try 78)
(comment
  ;; Solution

  (->> (range 0 2000)
       (filter y-path))
  ;; => 79

  (->> (iterate iterate-y {:y-pos 0 :y-vel 79})
       (filter #(= 0 (:y-vel %)))
       first)
  ;; => {:y-pos 3160, :y-vel 0}
  )

(def n (dec (* -1 (->> target :y (apply min)))))
(/ (* n (inc n)) 2)

;;; Part 2

(defn x-path [x-try]
  (->> (iterate iterate-x {:x-pos 0 :x-vel x-try})
       (take-while #(and (<= (:x-pos %) (-> target :x second))
                         (<= 0 (:x-vel %))))
       #_(some #(<= (-> target :x first)
                  (:x-pos %)
                  (-> target :x second)))))
(->> (range 100)
     (filter x-path))
(def x-try 10)

(->> [[23,-10]  [25,-9]   [27,-5]   [29,-6]   [22,-6]   [21,-7]   [9,0]     [27,-7]   [24,-5]
      [25,-7]   [26,-6]   [25,-5]   [6,8]     [11,-2]   [20,-5]   [29,-10]  [6,3]     [28,-7]
      [8,0]     [30,-6]   [29,-8]   [20,-10]  [6,7]     [6,4]     [6,1]     [14,-4]   [21,-6]
      [26,-10]  [7,-1]    [7,7]     [8,-1]    [21,-9]   [6,2]     [20,-7]   [30,-10]  [14,-3]
      [20,-8]   [13,-2]   [7,3]     [28,-8]   [29,-9]   [15,-3]   [22,-5]   [26,-8]   [25,-8]
      [25,-6]   [15,-4]   [9,-2]    [15,-2]   [12,-2]   [28,-9]   [12,-3]   [24,-6]   [23,-7]
      [25,-10]  [7,8]     [11,-3]   [26,-7]   [7,1]     [23,-9]   [6,0]     [22,-10]  [27,-6]
      [8,1]     [22,-8]   [13,-4]   [7,6]     [28,-6]   [11,-4]   [12,-4]   [26,-9]   [7,4]
      [24,-10]  [23,-8]   [30,-8]   [7,0]     [9,-1]    [10,-1]   [26,-5]   [22,-9]   [6,5]
      [7,5]     [23,-6]   [28,-10]  [10,-2]   [11,-1]   [20,-9]   [14,-2]   [29,-7]   [13,-3]
      [23,-5]   [24,-8]   [27,-9]   [30,-7]   [28,-5]   [21,-10]  [7,9]     [6,6]     [21,-5]
      [27,-10]  [7,2]     [30,-9]   [21,-8]   [22,-7]   [24,-9]   [20,-6]   [6,9]     [29,-5]
      [8,-2]    [27,-8]   [30,-5]   [24,-7]]
     (group-by first)
     (util/map-vals #(sort-by second %))
     (sort-by first))

(defn position [start-vel step]
  ;; pair-val * num steps / 2
  ;; ie step(startvel - step + 1) / 2
  (/ (* (+ start-vel (inc (- start-vel step)))
        step)
     2))

(defn x-pos [start-vel step]
  ;; velocity mins out at 0
  (position start-vel (min step start-vel)))

;; If (x-pos start-vel start-vel) < closest edge, impossible to hit target
;; Can't hit target if your first step takes you past the target
@(def possible-x-vels
  (->> (range 0 (-> target :x second inc))
       (filter #(>= (x-pos % %) (-> target :x first)))))

;; if vel is more negative than distance to target, will always miss
;; we know max vel from step 1: it's -lowest bound + 1
@(def possible-y-vels
   (let [lowest-target (->> target :y (apply min))]
     (range lowest-target
            (inc (* -1 lowest-target)))))

(* (count possible-y-vels)
   (count possible-x-vels))
;; => 46851
;; That seems brute-forceable

(defn solution [target]
  (let [x-vels (->> (range 0 (-> target :x second inc))
                    (filter #(>= (x-pos % %) (-> target :x first))))
        lowest-target (->> target :y (apply min))
        y-vels (range lowest-target
                      (inc (* -1 lowest-target)))]
    (for [x x-vels
          y y-vels]
      )))

;; step(startvel - step + 1) / 2 = pos
;; solve for step: s = 1/2 (-sqrt ((v + 1) ^2 - 8 p) + v + 1)
;; This isn't going to work, there are multiple solutions for many positions and there's lots of
;; imaginary numbers
(defn step [pos start-vel]
  (/ (+ (math/sqrt (+ (math/pow (inc start-vel) 2.)
                      (* -8 pos)))
        start-vel
        1)
   2))


(map #(position 3 %) (range 5))
(step -4 3)
(take 10 (iterate iterate-y {:y-pos 0 :y-vel 3}))