(ns advent-of-code.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data [[2 1 9 9 9 4 3 2 1 0]
           [3 9 8 7 8 9 4 9 2 1]
           [9 8 5 6 7 8 9 8 9 2]
           [8 7 6 7 8 9 6 7 8 9]
           [9 8 9 9 9 6 5 6 7 8]])

#_(def data
    (with-open [f (clojure.java.io/reader "resources/day9/input.txt")]
      (->> (line-seq f)
           (map #(->> (str/split % #"")
                      (map (fn [x] (Integer/parseInt x)))
                      vec))
           vec)))

(defn neighbor-coords [coord]
  (for [k [0 1]
        f [dec inc]]
    (update coord k f)))

(comment
  (neighbor-coords [0 0]))

(defn lowest? [coord]
  (every? #(< (get-in data coord)
              (get-in data % 99))
          (neighbor-coords coord)))

;; Part 1
(comment
  (->> (for [y (range (count data))
             x (range (count (first data)))]
         [y x])
       (filter lowest?)
       (map #(get-in data %))
       (map inc)
       (reduce +)))

;; Part 2
(defn minima [data]
  (->> (for [y (range (count data))
             x (range (count (first data)))]
         [y x])
       (filter lowest?)))

(defn render [{:keys [done]}]
  (reduce #(assoc-in %1 %2 "") data done))

(comment
  (render {:done [[0 9] [1 9]]})
  )

(defn flood-1 [{[coord & coords] :todo
                done :done}]
  (let [ns (->> coord
                neighbor-coords
                (filter #(> 9 (get-in data % 99)))
                set
                (#(set/difference % done)))]
    {:todo (into coords ns)
     :done (conj done coord)}))

(comment
  (flood-1 {:todo #{[0 9]}
          :done #{}}))

(defn flood [coord]
  (->> (iterate flood-1
                {:todo #{coord}
                 :done #{}})
       (drop-while #(seq (:todo %)))
       first
       :done))

(comment
  (-> (flood [0 9])
      render)

  (->> (map flood (minima data))
       (map count)
       (sort #(compare %2 %1))
       (take 3)
       (reduce *))
  ;; => 1269555
  )