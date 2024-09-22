(ns clj2023.day22 
  (:require [clj2023.util :refer [>>-> as->> fork spy]]
            [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [ubergraph.alg :as ug-alg]
            [ubergraph.core :as ug]))

(defn parse [raw]
  (map #(->> (re-seq #"\d+" %)
             (map parse-long)
             (split-at 3)
             (mapv vec))
       (str/split-lines raw)))

(defn bounds [f data]
  (->> data
       (mapcat identity)
       (reduce (partial map f))))

(comment
  @(def test-data
     (parse "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"))

  (def real-data
    (parse (slurp (io/resource "day22.txt"))))

  (bounds max real-data)
  ;; => (9 9 337)
  (bounds min real-data)
  ;; => (0 0 1)

  ;; Are initial zs unique?
  [(count real-data) (count (into #{}
                                  (comp (mapcat identity)
                                        (map #(nth % 2)))
                                  real-data))]
  ;; => [1439 336]
  ;; No
  ;; Intersections:
  (->> real-data (mapcat identity) (map #(nth % 2))
       frequencies (sort-by val >) (take 5))
  ;; => ([3 19] [165 18] [308 18] [201 18] [311 17])

  ;; How can there be no conflicts? Here's everything at z=3
  (->> real-data (filter #(or (= 3 (nth (first %) 2)) (= 3 (nth (second %) 2)))))
  ;; Oh right, 9x9 is pretty big, 81 possible coords

  ;; Does the lower z coord always appear first?
  (->> real-data (filter (fn [[[_ _ z1] [_ _ z2]]] (> z1 z2))))
  ;; => ()
  ;; Yes
  ;; So we can just sort on the first coord and run them in that order 
  )

(defn drop-brick [[tallest free] [[x1 y1 z1] [x2 y2 z2] :as bk']]
  ;; (prn tallest)
  ;; (prn bk')
  (let [[ht bks] (->> (m/submatrix tallest
                                   x1 (inc (- x2 x1))
                                   y1 (inc (- y2 y1)))
                      m/eseq
                      (group-by :ht)
                      (apply max-key key)
                      (>>-> update 1 set))
        z2' (+ ht (- z2 z1 -1))]
    (assert (< ht z1) (str "bk=" bk' " ht=" ht))
    [(m/set-selection tallest
                      (range x1 (inc x2)) (range y1 (inc y2))
                      {:ht z2' :bk bk'})
     ;; If a brick is supported by 2 or more bricks, then any one of those supporting bricks could
     ;; be removed safely. A brick is only "critical" if it is the only one supporting another brick.
     (-> free
         (disj (if (< 1 (count bks)) [] (:bk (first bks))))
         (conj bk'))]))

(defn drop-bricks [data]
  (let [[xb yb] (map inc (bounds max data))]
    (reduce drop-brick
            [(-> (repeat (* xb yb) {:ht 0})
                 (m/reshape [xb yb]))
             #{}]
            (sort-by #(get-in % [0 2]) data))))

(comment
  (->> (m/submatrix (m/reshape (map #(hash-map :bk :hi :ht %) (range 81)) [9 9])
                    2 2 4 2)
       m/eseq
       (apply max-key :ht))
  
  (->> (m/submatrix [[{:ht 0} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 1, :bk [[3 1 2] [4 1 2]]} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 1, :bk [[3 1 2] [4 1 2]]} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 0} {:ht 3, :bk [[5 2 1] [5 2 3]]} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 4, :bk [[5 7 2] [5 7 5]]} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 0}] [{:ht 0} {:ht 1, :bk [[7 1 1] [7 2 1]]} {:ht 1, :bk [[7 1 1] [7 2 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 0}] [{:ht 2, :bk [[8 0 2] [8 0 3]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[9 0 2] [9 1 2]]} {:ht 1, :bk [[9 0 2] [9 1 2]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 2, :bk [[9 8 2] [9 8 3]]} {:ht 0}]]
                    0 4
                    0 3)
       m/eseq
       (group-by :ht)
       (apply max-key key))

  (m/select (m/reshape (range 81) [9 9]) [4 5 6] (range 4 7)) 

  (doseq [[tallest free] (take 5 (drop 5 (drop-bricks real-data)))]
    ;; (m/pm tallest {:formatter (comp str :ht)})
    (prn free)
    (prn))
  
  (m/pm [[{:ht 0} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 1, :bk [[0 1 1] [0 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 0}] [{:ht 1, :bk [[1 0 1] [3 0 1]]} {:ht 1, :bk [[3 1 2] [4 1 2]]} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 1, :bk [[3 1 2] [4 1 2]]} {:ht 0} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[2 8 2] [4 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 0} {:ht 3, :bk [[5 2 1] [5 2 3]]} {:ht 0} {:ht 1, :bk [[2 4 1] [5 4 1]]} {:ht 0} {:ht 0} {:ht 4, :bk [[5 7 2] [5 7 5]]} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 1, :bk [[3 9 1] [5 9 1]]}] [{:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 0}] [{:ht 0} {:ht 1, :bk [[7 1 1] [7 2 1]]} {:ht 1, :bk [[7 1 1] [7 2 1]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 1, :bk [[5 8 2] [7 8 2]]} {:ht 0}] [{:ht 2, :bk [[8 0 2] [8 0 3]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0}] [{:ht 1, :bk [[9 0 2] [9 1 2]]} {:ht 1, :bk [[9 0 2] [9 1 2]]} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 0} {:ht 2, :bk [[9 8 2] [9 8 3]]} {:ht 0}]]
        {:formatter (comp str :ht)})
  
  (-> (drop-bricks (parse "0,0,1~1,0,1
0,1,1~0,1,2
0,0,5~0,0,5
0,0,4~0,1,4"))
      second
      #_count)
  )
;; A brick can be supported by 2 or more bricks. Is that unhandled?
;; Yes, if a brick is supported by 2 others, those 2 should be considered "removable"


(comment
  (-> (drop-bricks test-data)
      second
      count)
    ;; => 5
  
  (-> (drop-bricks real-data)
      second
      count)
  )

;; Pt 2

;; Interesting case:
;; z -- a -- b -- d
;;        \-- c -/
;; If a is removed, d falls. If b or c is removed, d does not fall
;; Given:
;; - 2 bricks B1 and B2
;; - The set ancestors of B1, A1
;; - The set of descendants of B2, D2
;; - The set of ancestors of B2, A2
;; B1 falls when B2 is removed IFF A1 - A2 - D2 - {B2} = 0

(defn drop-brick2 [[tallest graph] [[x1 y1 z1] [x2 y2 z2] :as bk']]
  ;; (prn tallest)
  ;; (prn bk')
  (let [[ht bks] (->> (m/submatrix tallest
                                   x1 (inc (- x2 x1))
                                   y1 (inc (- y2 y1)))
                      m/eseq
                      (group-by :ht)
                      (apply max-key key)
                      (>>-> update 1 set))
        z2' (+ ht (- z2 z1 -1))]
    (assert (< ht z1) (str "bk=" bk' " ht=" ht))
    [(m/set-selection tallest
                      (range x1 (inc x2)) (range y1 (inc y2))
                      {:ht z2' :bk bk'})
      (->> (map :bk bks)
           (filter some?)
           (map #(vector % bk'))
           (apply ug/add-edges (ug/add-nodes graph bk')))]))

(defn drop-bricks2 [data]
  (let [[xb yb] (map inc (bounds max data))]
    (reduce drop-brick2
            [(-> (repeat (* xb yb) {:ht 0})
                 (m/reshape [xb yb]))
             (ug/digraph)]
            (sort-by #(get-in % [0 2]) data))))

(defn ancestors
  ([g start] (ancestors #{} g [start]))
  ([result g starts]
   (let [ancs (mapcat identity (keep #(ug/predecessors g %) starts))]
     (into result (concat ancs (mapcat #(ancestors g %) ancs))))))

(defn descendants
  ([g start] (descendants #{} g [start]))
  ([result g starts]
   (let [ancs (mapcat identity (keep #(ug/successors g %) starts))]
     (into result (concat ancs (mapcat #(descendants g %) ancs))))))

(comment
  (count (ancestors g [[1 0 1] [1 2 1]]))
  (count (descendants g [[1 1 8] [1 1 9]]))
  )

(comment
  (def r (drop-bricks2 real-data))
  (def g (second r))
  (->> (ug/nodes (second r))
       (map (partial ug-alg/longest-shortest-path (second r)))
       (map ug-alg/edges-in-path)
       (map count)
       frequencies
       (sort (comp - compare))
       (fork #(concat %1 %2) #(take 5 %) #(reverse (take 5 (reverse %)))))
  ;; => ([142 2]
  ;;     [141 5]
  ;;     [140 8]
  ;;     [139 9]
  ;;     [138 7]
  ;;     [4 12]
  ;;     [3 13]
  ;;     [2 9]
  ;;     [1 15]
  ;;     [0 23])

  ;; Dependency graphs are short-ish, set approach will hopefully be fine?

  ;; z -- a -- b -- d
  ;;       \-- c -/
  ;; Given:
  ;; - 2 bricks B1 and B2
  ;; - The set descendants of B1, D1
  ;; - The set of ancestors of B2, A2
  ;; - The set of descendants of B2, D2
  ;; B1 falls when B2 is removed IFF D1 - D2 - A2 - {B2} = 0
  (def g (second r))
  (def g (second (drop-bricks2 test-data)))
  (ug/pprint g)
  (->> (for [n2 (ug/nodes g)
             n1 (ancestors g (spy n2))
             :let [n1-desc (descendants g n1)
                   n2-anc (ancestors g n2)
                   n2-desc (descendants g n2)]
             :when (empty? (set/difference n1-desc n2-desc n2-anc #{n2}))]
         [n1 n2])
       count)
  ;; This is too slow
  ;; Let's try doing it in 1 traversal per node  
  )

(defn supported-by [g node]
  (->> (ug-alg/bf-traverse g node)
       (reduce (fn [seen n]
                 (cond-> seen
                   (every? seen (ug/predecessors g n))
                   (conj n)))
               #{node})
       (>>-> disj node)))

(comment
  (count (supported-by g [[1 0 1] [1 2 1]]))
  ;; => 6
  (count (supported-by g [[1 1 8] [1 1 9]]))
  ;; => 0
  (count (supported-by g [[0 0 2] [2 0 2]]))
  ;; => 2
  (count (supported-by g [[0 1 6] [2 1 6]]))
  ;; => 1
  ;; Looks good

  (let [[_ g] (drop-bricks2 test-data)]
    (->> (ug/nodes g)
         (map (partial supported-by g))
         (map count)
         (apply +)))
  
  (let [[_ g] (drop-bricks2 real-data)]
    (->> (ug/nodes g)
         (map (partial supported-by g))
         (map count)
         (apply +)))
  ;; => 34631
  ;; Too low
  )

(defn pt2 [data]
  (let [[_ g] (drop-bricks2 (parse data))]
    (->> (ug/nodes g)
         (map (partial supported-by g))
         (map count)
         (apply +))))

(comment
  (pt2 "0,0,2~0,0,4 a
1,0,3~2,0,3 b
1,0,4~1,0,5 c
0,0,6~1,0,6 d")

  (def g (second (drop-bricks2 (parse "0,0,2~0,0,4 a
                               1,0,3~2,0,3 b
                               1,0,4~1,0,5 c
                               0,0,6~1,0,6 d"))))
  (supported-by g [[1 0 3] [2 0 3]])
  (ug/pprint g)

  ;; Can I get the pt1 answer? 501
  (def data (parse "0,0,2~0,0,4
1,0,3~2,0,3
1,0,4~1,0,5
0,0,6~1,0,6"))

  (let [[_ g] (drop-bricks2 real-data)]
    ;; (ug/pprint g)
    (->> (ug/nodes g)
         (filter #(->> (ug/successors g %)
                       (map (partial ug/predecessors g))
                       (map count)
                       (every? (partial < 1))))
         count))
  ;; Wrong! Ok, so there's a bug somewhere
  (pt2 (slurp (io/resource "day22.txt")))
  )
;;3 d-d
;;2 c b
;;1 a b
;;0 0 1
;;z x
;; We're missing a entirely, graph construction is wrong
;; Because we only add nodes when we add edges, we miss adding nodes that never get any edges

;; Well that was ONE issue. Apparently we still have more.
