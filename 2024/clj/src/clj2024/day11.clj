(ns clj2024.day11
  (:require [clojure.string :as str]))

(defn do-1 [n]
  (cond
    (zero? n) [1]
    (even? (count (str n))) (let [s (str n)] (map (comp parse-long str/join) (split-at (/ (count s) 2) s)))
    :else [(* 2024 n)]))

(defn pt1 [init n]
  (-> (iterate #(mapcat do-1 %) init)
      (nth n)
      count))

(defn parse [raw]
  (->> raw
       (re-seq #"\d+")
       (map parse-long)))

(comment
  (time (pt1 [125 17] 25))
  (pt1 (parse (slurp "resources/day11.txt")) 25)
  (pt1 (parse (slurp "resources/day11.txt")) 75)
  (doseq [n (range 35)]
    (prn (pt1 (parse (slurp "resources/day11.txt")) n)))
  )

(def ^:dynamic *seen*)
(def ^:dynamic *hits*)

(defn go [n depth max-depth]
  (cond
    (get @*seen* [n depth]) (do (swap! *hits* inc) (get @*seen* [n depth]))
    (>= depth max-depth) 1
    :else (let [depth' (inc depth)
                nums (do-1 n)
                rslts (map #(go % depth' max-depth) nums)]
            (dorun (mapv #(swap! *seen* assoc [%1 depth'] %2) nums rslts))
            (apply + rslts))))

(defn pt2 [nums reps]
  (binding [*seen* (atom {})
            *hits* (atom 0)]
    (apply + (map #(go % 0 reps) nums))))

(comment
  (time (pt2 (parse (slurp "resources/day11.txt")) 75))

  (time
   (binding [*seen* (atom {})
             *hits* (atom 0)]
     (apply + (map #(go % 0 6) [125 17]))
     @*hits*))

  (doseq [n (range 1 30)]
    (binding [*seen* (atom {})
              *hits* (atom 0)]
      (let [r (apply + (map #(go % 0 n) (parse (slurp "resources/day11.txt"))))]
        (prn r)
        r)))
  )