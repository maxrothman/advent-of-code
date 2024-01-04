(ns clj2023.day15
  (:require [clj2023.util :refer [spy]]
            [clojure.string :as str]
            [medley.core :as m]
            [clojure.java.io :as io]
            [flatland.ordered.map :as omap]))

(defn myhash [s]
  (reduce #(-> %2 int (+ %1) (* 17) (rem 256))
              0 s))

(def test-data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn pt1 [data]
  (apply + (map myhash (str/split data #","))))

(comment
  (pt1 test-data)
  (pt1 (slurp (io/resource "day15.txt")))
  ;; Had to manually remove the newline at the end of the file. That's not cheating, right?
  )

(defn map-replace
  "Replace the value at k with v in ordered map m, in both value and order"
  [m k v]
  (if-let [[i _] (get (.backing-map m) k)]
    (omap/->OrderedMap (-> (dissoc (.backing-map m) k) (.cons (omap/entry k v i)))
                       (.assoc (.order m) i (m/map-entry k v)))
    (assoc m k v)))

(comment
  (-> (omap/ordered-map :a :x :b :y :c :z)
      (map-replace :d :zz))
  (-> (omap/ordered-map :a :x :b :y :c :z)
      (dissoc :a)
      (map-replace :b :zz))
  )

(defn nil-safe [f x]
  (if (nil? x)
    x
    (f x)))

(defn parse [raw]
  (->> (str/split raw #",")
       (map #(subvec (re-matches #"^([a-z]+)(=|-)([0-9]+)?" %) 1))
       (map #(update % 2 (partial nil-safe parse-long)))))

(defn interpret-1 [hmap [label op v]]
  (case op
    "-" (update hmap (myhash label) 
                (fnil dissoc (omap/ordered-map)) label)
    "=" (update hmap (myhash label) 
                (fnil map-replace (omap/ordered-map)) label v)))

(defn interpret [instrs]
  (reduce interpret-1 {} instrs))

(defn score [m]
  (->> (for [[hsh omap] m
             [i [_ v]] (map-indexed vector omap)]
         (* (inc hsh) (inc i) v))
       (apply +)))

(defn pt2 [raw]
  (score (interpret (parse raw))))

(comment
  (pt2 test-data)
  (pt2 (slurp (io/resource "day15.txt")))
  )