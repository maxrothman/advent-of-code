(ns advent-of-code.day15
  (:require [clojure.string :as str]
            [advent-of-code.util :as util]
            [clojure.string :as str]))

(def test-input
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn parse [input]
  (->> input
       str/split-lines
       (mapv (partial mapv (comp #(Long/parseLong %) str)))))

(defn shape [mat]
  [(count mat)
   (count (first mat))])

(defn neighbors [matrix coord]
  (->> [[1 0] [0 1] [-1 0] [0 -1]]
       (map #(mapv + coord %))
       (filter #(get-in matrix %))))

(comment
  (neighbors matrix [0 0])
  ;; => ([1 0] [0 1])
  )

(defn end? [matrix coord]
  (= coord [(dec (count matrix))
            (dec (count (matrix 0)))]))

(defn total-cost [path]
  ;; Trying a transducer for performance
  (transduce (map #(nth % 1)) + path)
  #_(->> path
         (map second)
         (reduce +)))

(defn enqueue [paths path]
  (->> (conj paths path)
       (sort-by total-cost)))

(comment
  (enqueue [[[[0 0] 2] [[0 1] 3]]
            [[[0 0] 2] [[1 0] 4]]]
           [[[1 1] 3]])
  ;; => ([[[1 1] 3]]
  ;;     [[[0 0] 2] [[0 1] 3]]
  ;;     [[[0 0] 2] [[1 0] 4]])
  )

(defn step [matrix {[[[coord _] :as path] & paths] :paths
                    seen :seen}]
  ;; We check seen on both coord and neighbors b/c we might've previously found
  ;; a shorter path to the same coord on a completely different path
  (if (seen coord)
    {:paths paths
     :seen seen}
    (->> (neighbors matrix coord)
         (remove seen)
         (map #(vector % (get-in matrix %)))  ;Add cost
         (map #(conj path %))  ;Construct path
         (reduce enqueue paths)
         (hash-map :seen (conj seen coord)
                   :paths #_threaded))))

(def counter (atom 1000))
(defn trace-count [x]
  (swap! counter inc)
  x)

(defn faster-recursive-step [matrix seen paths]
  (swap! counter dec)
  (if (> 0 @counter)
    (do
      (prn "timed out!")
      (first paths))
    (let [[[coord _] :as path] (first paths)
          next-paths (disj paths path)]
      (cond
        (end? matrix coord) path
        (seen coord) (recur matrix seen next-paths)
        :else (->> (neighbors matrix coord)
                   (remove seen)
                   (map #(vector % (get-in matrix %)))  ;Add cost
                   (map #(conj path %))  ;Construct path
                   (into next-paths)
                   (recur matrix (conj seen coord) #_"next-paths threaded"))))))

(defn compare-by-total-cost [p1 p2]
  (let [r (compare (total-cost p1) (total-cost p2))]
    (if (zero? r)
      (compare (first p1) (first p2))
      r)))

(comment
  (def p1 [[[0 0] 1] [[3 3] 2]])
  (def p2 [[[1 1] 0] [[5 5] 1]])
  (compare [1] [2])
  (def sst (-> (sorted-set-by compare-by-total-cost p2 p1)
               (conj [[[2 2] 0]])))

  (compare-by-total-cost p1 p2))

;; Optimization takeaways:
;; - tail recursion is faster than iterate
;; - sorted-set is faster than sort-by
;; - improvement between each was at least an order of magnitude

(defn render [matrix path]
  (let [path (into {} path)
        [ymax xmax] (shape matrix)]
    (str/join
     "\n"
     (for [y (range ymax)]
       (str/join
        ""
        (for [x (range xmax)]
          (if (contains? path [y x])
            (str (get-in matrix [y x]) "|")
            (str (get-in matrix [y x]) " "))))))))

(comment
  (def matrix (parse test-input))

  (step matrix {:paths '(([[0 0] 0])), :seen #{}})
  ;; => (([[0 1] 1] [[0 0] 0]) ([[1 0] 1] [[0 0] 0]))

  (-> (iterate (partial step matrix) {:paths '(([[0 0] 0]))
                                      :seen #{}})
      (nth 50)
      :paths
      first
      #_(#(take 5 %))
      #_((comp #_(partial end? matrix) first first first)))

  ;; An exhaustive search, even breadth-first by lowest weight, isn't going to complete in a
  ;; reasonable amount of time. There's too many paths.
  ;; It gets close around 200 iterations at 6,0/7,1, but after 300 iterations it goes back to
  ;; messing around at 1,1 and 2,2.
  ;; Need to implement A* or something to reduce the search space.
  ;; Or just change priority fn? Could use longest path, or distance from goal
  ;;
  ;; Do not return to coords already explored in any other path, if you’ve seen
  ;; it, you’ve already discovered the shortest path to it. “Seen” added to when
  ;; exploring, not when adding neighbors. Check both when adding neighbors and
  ;; exploring. Proof: by exploring lowest weight first, if two paths end at the
  ;; same point, the shorter path will always be explored first. Therefore any
  ;; other path we find cannot be shorter, so don’t bother exploring it.

  (-> (util/until (comp (partial end? matrix) first first first :paths)
                  (partial step matrix)
                  {:paths '(([[0 0] 0])), :seen #{}})
      :paths
      first
      total-cost)
  ;; => 40


  (-> (iterate (partial step matrix) {:paths '(([[0 0] 0]))
                                      :seen #{}})
      (nth 1000)
      :paths
      first
      time
      #_(#(take 5 %))
      #_((comp #_(partial end? matrix) first first first)))
  ;; 2756.964405 msecs

  (time (faster-recursive-step matrix #{} '(([[0 0] 0]))))
  ;; 425.395683 msecs

  (def matrix (parse (slurp "resources/day15/input.txt")))
  #_(-> (util/until (comp (partial end? matrix) first first first :paths)
                    (partial step matrix)
                    {:paths '(([[0 0] 0])), :seen #{}})
        :paths
        first
        total-cost)

  (reset! counter 23000)
  (->> (faster-recursive-step matrix
                              #{}
                              (sorted-set-by compare-by-total-cost
                                             '([[0 0] 0])))
       time
       #_total-cost
       #_(map reverse)
       #_(render matrix))
  (def shortest-path *1)
  (- 23000 @counter)
  ;; => 18475
  (total-cost shortest-path)
  ;; => 613

  ;; Part 1 complete!

  (spit "resources/day15/part1-solution" shortest-path)
  (spit "resources/day15/part1-rendered" (render matrix shortest-path))


  (.. java.lang.ProcessHandle (current) (pid)))

;; Part 2

;; Graph is way too big, try A*? Metric could just be simple distance from end +
;; length
;; cost(path) = (total-cost path) + (distance path end)
;; Looking at part1 path, seems like they won't take us on too many twists & turns

(defn spy [x] (prn x) x)

(defn a*-comparator [matrix]
  (let [end (shape matrix)
        distance (fn [[[coord _]]]
                   (->> (mapv - end coord)
                        (apply +)))
        cost #(util/fork + total-cost distance %)]
    (fn compare-a* [p1 p2]
      (let [c (compare (cost p1) (cost p2))]
        (if (zero? c)
          (compare (first p1) (first p2))
          c)))))

(comment
  (def a*-cmp (a*-comparator matrix))
  (compare 0 1)
  (a*-cmp [[[3 4] 4]] [[[56 86] 139]])
  ;; That puts them even, tiebroken by path itself
  )


(comment
  (def part-1-shortest-path shortest-path)

  ;; Compare perf of original solution
  (reset! counter 23000)
  (->> (faster-recursive-step matrix
                              #{}
                              (sorted-set-by compare-by-total-cost
                                             '([[0 0] 0])))
       time
       total-cost)
  ;; => 613
  ;; "Elapsed time: 35790.005674 msecs"
  (- 23000 @counter)
  ;; => 18475

  (reset! counter 23000)
  (->> (faster-recursive-step matrix
                              #{}
                              (sorted-set-by (a*-comparator matrix)
                                             '([[0 0] 0])))
       time
       total-cost)
  ;; => 613
  ;; "Elapsed time: 40759.809336 msecs"
  (- 23000 @counter)
  ;; => 18454

  ;; With transducified total-cost
  ;; "Elapsed time: 17919.319663 msecs"

  ;; With nth in total-cost
  ;; "Elapsed time: 6836.029933 msecs"

  ;; A* version
  ;; "Elapsed time: 9951.637824 msecs"
  )

;; Trying something else: if we only need the total cost, we can keep track of
;; that rather than the whole path. Much smaller collection of paths, no need to
;; iterate over paths in the tight loop
;; 
;; From reddit:
;; There's another approach. Keep a list of points to be expanded that are at
;; distance i from the start for each i. When you need the minimum, pop an item
;; from the first nonempty list. Given that the largest price is 9, you only
;; ever have 10 of those lists active. That means you can just cycles through
;; them. Practically this is faster than anything else I've seen. Asymptotically
;; all operations are O (1)

;; This fn is highly optimized because it's called on every conj and disj to the
;; sorted set, it's the hot spot of the entire algorithm
(defn a*-comparator-again [matrix]
  (let [zero ^java.lang.Integer (int 0)
        [endy endx] (shape matrix)
        distance (fn [[y x]] (+ (- endy y) (- endx x)))

        cost #(nth % 1)] ;; A* appears to be slower?
    #_(fn [[coord cst]] (+ cst (distance coord)))
    (fn compare-a* [[c1 :as p1] [c2 :as p2]]
      (let [result ^java.lang.Integer (compare (cost p1) (cost p2))]
        (if (.equals result zero)
          (compare c1 c2)
          result)))))

(comment
  (->> (step-again matrix
                   #{}
                   (sorted-set-by (a*-comparator-again matrix)
                                  [[0 0] 0]))
       time)

  (compare 1 10)
  ((a*-comparator-again matrix) [[0 0] 0] [[1 1] 2])
  (first (sorted-set-by (a*-comparator-again matrix)
                        [[0 0] 0], [[1 1] 2])))

;; A secondary bottleneck, also fairly highly optimized
(defn neighbors-td [matrix coord]
  (into []
        (comp (map #(let [[cy cx] coord
                          [y x] %]
                      [(+ y cy) (+ x cx)]))
              (filter #(get-in matrix %)))
        [[1 0] [0 1] [-1 0] [0 -1]]))

(defn step-again [matrix seen coords]
  (let [[coord cost :as point] (first coords)
        next-coords (disj coords point)]
    (cond
      (end? matrix coord) cost
      (seen coord) (recur matrix seen next-coords)
      :else (recur matrix
                   (conj seen coord)
                   (into next-coords
                         (comp (remove seen)
                               (map #(vector % (+ cost (get-in matrix %)))))
                         (neighbors-td matrix coord)))
      ;; Saves 50ms on part 1 input
      #_(->> (neighbors matrix coord)
             (remove seen)
             (map #(vector % (+ cost (get-in matrix %))))  ;Add new cost onto total
             (into next-coords)
             (recur matrix (conj seen coord) #_"next-coords threaded")))))

(comment
  (->> (step-again matrix
                   #{}
                   (sorted-set-by (a*-comparator-again matrix)
                                  [[0 0] 0]))
       time)
  ;; "Elapsed time: 2754.347152 msecs"
  ;; Well that's a big improvement

  ;; Down to 230ms! Time to try a bigger matrix. For 5x bigger in each
  ;; dimension, should take 25x the time, or ~6s
  )

(defn bigger-matrix [mat]
  (let [[tgt-y tgt-x] (mapv #(* 5 %) (shape mat))
        [y-size x-size] (shape mat)]
    (vec (for [y (range tgt-y)]
           (vec (for [x (range tgt-x)]
                  (-> (get-in mat [(mod y y-size) (mod x x-size)])
                      dec
                      (+ (quot y y-size) (quot x x-size))
                      (mod 9)
                      (+ 1))))))))

(comment
  (for [x (range 1 10)] (+ 1 (mod (dec x) 3)))
  ;; => (1 2 3 1 2 3 1 2 3)

  (def test-mat (parse test-input))
  (def expected
    (parse (str/join "\n"
                     ["11637517422274862853338597396444961841755517295286"
                      "13813736722492484783351359589446246169155735727126"
                      "21365113283247622439435873354154698446526571955763"
                      "36949315694715142671582625378269373648937148475914"
                      "74634171118574528222968563933317967414442817852555"
                      "13191281372421239248353234135946434524615754563572"
                      "13599124212461123532357223464346833457545794456865"
                      "31254216394236532741534764385264587549637569865174"
                      "12931385212314249632342535174345364628545647573965"
                      "23119445813422155692453326671356443778246755488935"
                      "22748628533385973964449618417555172952866628316397"
                      "24924847833513595894462461691557357271266846838237"
                      "32476224394358733541546984465265719557637682166874"
                      "47151426715826253782693736489371484759148259586125"
                      "85745282229685639333179674144428178525553928963666"
                      "24212392483532341359464345246157545635726865674683"
                      "24611235323572234643468334575457944568656815567976"
                      "42365327415347643852645875496375698651748671976285"
                      "23142496323425351743453646285456475739656758684176"
                      "34221556924533266713564437782467554889357866599146"
                      "33859739644496184175551729528666283163977739427418"
                      "35135958944624616915573572712668468382377957949348"
                      "43587335415469844652657195576376821668748793277985"
                      "58262537826937364893714847591482595861259361697236"
                      "96856393331796741444281785255539289636664139174777"
                      "35323413594643452461575456357268656746837976785794"
                      "35722346434683345754579445686568155679767926678187"
                      "53476438526458754963756986517486719762859782187396"
                      "34253517434536462854564757396567586841767869795287"
                      "45332667135644377824675548893578665991468977611257"
                      "44961841755517295286662831639777394274188841538529"
                      "46246169155735727126684683823779579493488168151459"
                      "54698446526571955763768216687487932779859814388196"
                      "69373648937148475914825958612593616972361472718347"
                      "17967414442817852555392896366641391747775241285888"
                      "46434524615754563572686567468379767857948187896815"
                      "46833457545794456865681556797679266781878137789298"
                      "64587549637569865174867197628597821873961893298417"
                      "45364628545647573965675868417678697952878971816398"
                      "56443778246755488935786659914689776112579188722368"
                      "55172952866628316397773942741888415385299952649631"
                      "57357271266846838237795794934881681514599279262561"
                      "65719557637682166874879327798598143881961925499217"
                      "71484759148259586125936169723614727183472583829458"
                      "28178525553928963666413917477752412858886352396999"
                      "57545635726865674683797678579481878968159298917926"
                      "57944568656815567976792667818781377892989248891319"
                      "75698651748671976285978218739618932984172914319528"
                      "56475739656758684176786979528789718163989182927419"
                      "67554889357866599146897761125791887223681299833479"])))

  (= expected (bigger-matrix test-mat))
  
  (let [mat (bigger-matrix matrix)]
    (time (step-again mat
                      #{}
                      (sorted-set-by (a*-comparator-again mat)
                                     [[0 0] 0]))))
  ;; Elapsed time: 6617.157126 msecs
  ;; => 2899
  ;; As predicted!
  )