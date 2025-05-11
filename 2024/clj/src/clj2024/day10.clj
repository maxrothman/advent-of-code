(ns clj2024.day10
  (:require [clojure.string :as str]
            [medley.core :refer [find-first indexed map-vals]]))

(def test-raw
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn parse [raw]
  (->> raw
       (str/split-lines)
       (mapv #(mapv (comp parse-long str) %))))

(defn neighbors [grid [y x]]
  (let [v (get-in grid [y x])]
    (->> [[y (inc x)]
          [y (dec x)]
          [(inc y) x]
          [(dec y) x]]
         (filter #(let [nv (get-in grid %)]
                    (and (some? nv)
                         (= (dec nv) v)))))))

;; todo-paths: '({:start pt, :end pt})
(defn step [grid [[cur & todo-paths] done-paths]]
  (if (= 9 (get-in grid (:end cur)))
    [todo-paths (conj done-paths cur)]
    [(->> (neighbors grid (:end cur))
          (map #(assoc cur :end %))
          (into todo-paths))
     done-paths]))

(defn starts [grid]
  (for [[y row] (indexed grid)
        [x v] (indexed row)
        :when (zero? v)]
    [y x]))

(defn sln [grid done-init]
  (->> (iterate (partial step grid)
                [(->> (starts grid) (map #(array-map :start % :end %)))
                 done-init])
       (find-first (comp empty? first))
       second
       (group-by :start)
       vals
       (map count)
       (apply +)))

(comment
  (sln (parse test-raw) #{})
  (sln (parse (slurp "resources/day10.txt")) #{})
  (sln (parse test-raw) '())
  (sln (parse (slurp "resources/day10.txt")) '())

  (def test (parse test-raw))
  (->> (iterate (partial step test)
                [(->> (starts test) (map #(array-map :start % :end %)))
                 #{}])
       #_#_(take 11) doall
       (drop-while first) first
       second
       (group-by :start)
       (map-vals count)
       vals sort (= [1 3 3 3 5 5 5 5 6]))
  )