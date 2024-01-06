(ns clj2023.day16
  (:require [clj2023.util :refer [>>->]]
            [clojure.core.match :refer [match]]
            [clojure.core.matrix :as m]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [medley.core :refer [take-upto]]))

(def test-data
  ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn parse [raw]
  (mapv vec (str/split-lines raw)))

(defn splitflect [dir mirror]
  (match [(mapv abs dir) mirror]
    [[0 1] \|] [[1 0] [-1 0]]
    [[1 0] \-] [[0 1] [0 -1]]
    [_ \/] [(mapv - (reverse dir))]
    [_ \\] [(vec (reverse dir))]
    :else [dir]))

;; Reduces size of output, making it easier to 
(defn jump [seen coord dir]
  )

(defn simulate [grid start dir]
  (iterate #_#_until (comp empty? second)
   (fn [[seen [[coord dir] & rst]]]
     (if (seen [coord dir])
       [seen rst]
       [(conj seen [coord dir])
        (into rst
              (let [new-coord (mapv + coord dir)
                    v (get-in grid new-coord)]
                (when v
                  (map #(vector new-coord %) (splitflect dir v)))))]))
   [#{} (list [start dir])]))

(defn pt1 [data]
  (->> (simulate (parse data) [0 0] [0 1])
       first
       (map first)
       set
       count))

(defn coords->mat [m coords cur]
  (-> (reduce #(assoc-in %1 %2 \#) m coords)
      (assoc-in cur \O)))

(comment
  ;; seen: #{[[x y] dir]}
  ;; dir: [x y] to add
  ;; todo: [[x y] dir]
  
  (let [data (parse (slurp (io/resource "day16.txt")))
        sim (take 100 (simulate data [0 0] [0 1]))
        seens (->> sim (map first) (map (partial map first)))
        curs (->> sim (map second) (map (partial map first)) (map first))]
    (dorun (->> (map (comp m/pm (partial coords->mat data))
                     seens curs)
                (map-indexed #(do %2 (prn %1))))))

  (let [data (parse test-data #_(slurp (io/resource "day16.txt")))
        sim (simulate data [0 0] [0 1])
        seen (->> sim first (map first))]
    (m/pm (coords->mat data seen [0 0])))

  (defn spyc [m x]
    (prn m)
    x)
  ;; To use, replace the `until` in simulate with `iterate`
  (->> (simulate (parse (slurp (io/resource "day16.txt"))) [0 0] [0 1])
       (take-upto (comp empty? second))
       (spyc "assoc")
       (assoc {:grid (m/emap str (parse (slurp (io/resource "day16.txt"))))} :moves)
       (spyc "json")
       (>>-> json/write-json-str {:indent-str " "})
       (spyc "str")
       (str "window.DATA = ")
       (spyc "spit")
       (spit "src/clj2023/day16/data.js"))

  (pt1 test-data)
  (pt1 (slurp (io/resource "day16.txt")))
  )
