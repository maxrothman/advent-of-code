(ns clj2023.day16
  (:require [charred.api :as json]
            [clj2023.util :refer [>>-> as->>]]
            [clj2023.util :refer [until]]
            [clojure.core.match :refer [match]]
            [clojure.core.matrix :as m]
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

(defn simulate* [grid [seen [[coord dir] & rst]]]
  (if (seen [coord dir])
    [seen rst]
    [(conj seen [coord dir])
     (into rst
           (let [new-coord (mapv + coord dir)
                 v (get-in grid new-coord)]
             (when v
               (map #(vector new-coord %) (splitflect dir v)))))]))

(defn simulate [grid start dir]
  (#_iterate until (comp empty? second)
             (partial simulate* grid)
             [#{} (list [start dir])]))

(defn count-seen [simulate-output]
  (->> simulate-output
       first
       (map first)
       set
       count
       ;; We added an extra cell in when we started out-of-frame
       dec))

(defn pt1 [data]
  (->> (simulate (parse data) [0 -1] [0 1])
       count-seen))

(defn coords->mat [m coords cur]
  (-> (reduce #(assoc-in %1 %2 \#) m coords)
      (assoc-in cur \O)))

(comment
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
  (let [data (parse test-data #_(slurp (io/resource "day16.txt")))]
    (->> (simulate data [0 0] [0 1])
        (take-upto (comp empty? second))
        (partition-all 500)
        #_#_first
        (spyc "assoc")
        (assoc {:grid (m/emap str data)} :moves)
        (spyc "json")
        (>>-> json/write-json-str {:indent-str " "})
        (spyc "str")
        (str "window.DATA = ")
        (spyc "spit")
        (spit "src/clj2023/day16/data.js")))

  (pt1 test-data)
  ;; => 46
  
  (time (pt1 (slurp (io/resource "day16.txt"))))
  ;; => 7060 
  )

(defn pt2 [data]
  (let [parsed (parse data)]
    (->> (map #(vector [%1 -1] [0 1]) (range (m/column-count parsed)))
         (concat (map #(vector [%1 (m/row-count parsed)] [0 -1])
                      (range (m/column-count parsed))))
         (concat (map #(vector [-1 %1] [1 0])
                      (range (m/row-count parsed))))
         (concat (map #(vector [(m/column-count parsed) %1] [-1 0])
                      (range (m/row-count parsed))))
         (map #(count-seen (apply simulate parsed %)))
         (apply max))))

(comment
  (pt2 test-data)
  ;; => 51
  
  (time (pt2 (slurp (io/resource "day16.txt"))))
  )
