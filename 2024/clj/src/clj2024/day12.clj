(ns clj2024.day12
  (:require
   [clj2024.util :refer [fork map2 spy]]
   [clojure.set :as set]
   [clojure.string :as str]
   [medley.core :refer [find-first least]]))

(def test
  (str/split-lines
   "AAAA
BBCD
BBCC
EEEC"))

(defn neighbors [[y x]]
  [[y (inc x)]
   [y (dec x)]
   [(inc y) x]
   [(dec y) x]])

(defn region-neighbors [grid pt]
  (let [v (get-in grid pt)]
    (->> (neighbors pt)
         (filter #(let [nv (get-in grid %)]
                    (and (some? nv)
                         (= v nv)))))))

(defn flow-1 [grid [[pt & pts] found]]
  [(into pts (remove found (region-neighbors grid pt)))
   (conj found pt)])

(defn flow [grid pt]
  (->> (iterate (partial flow-1 grid) [[pt] #{}])
       (find-first (comp empty? first))
       second))

(defn perimeter [region]
  (->> (for [[y x] region
             :let [outside-nbrs (remove region [[y (inc x)]
                                                [y (dec x)]
                                                [(inc y) x]
                                                [(dec y) x]])]]
         (count outside-nbrs))
       (apply +)))

(defn regions [grid]
  (loop [todo (set (for [y (range (count grid))
                         x (range (count (first grid)))]
                     [y x]))
         regions '()]
    (if-let [pt (first todo)]
      (let [rgn (flow grid pt)]
        (recur (apply disj todo pt rgn)
               (conj regions rgn)))
      regions)))

(defn pt1 [grid]
  (let [rgns (regions grid)]
    (apply + (map (partial fork * #(perimeter grid %) count) rgns))))

(comment
  (pt1 (str/split-lines "RRRRIICCFF
  RRRRIICCCF
  VVRRRCCFFF
  VVRCCCJFFF
  VVVVCJJCFE
  VVIVCCJJEE
  VVIIICJJEE
  MIIIIIJJEE
  MIIISIJEEE
  MMMISSJEEE"))

  (pt1 (str/split-lines (slurp "resources/day12.txt"))))

;;; Pt2

(defn outside-neighbors [region]
  (set (for [pt region
             nbr (remove region (neighbors pt))]
         nbr)))

(defn add-vec [pt v]
  (mapv + pt v))

(defn flood-side-in-dir [all-nbrs dir pt]
  (loop [found #{}, p pt]
    (if (all-nbrs p)
      (recur (conj found p) (add-vec p dir))
      found)))

(defn flood-side [all-nbrs pt v-h]
  (apply set/union
         (for [dir [v-h (mapv - v-h)]]
           (flood-side-in-dir all-nbrs dir pt))))

(comment
  (flood-side (outside-neighbors #{[2 2] [2 3] [3 3] [1 2]}) [1 0] [3 4])
  (flood-side-in-dir (outside-neighbors #{[2 2] [2 3] [3 3] [1 2]})
                     [2 4]
                     [1 0]))
;; TODO: process for finding sides
;; - make a set of neighbor-dirs to try (i.e. [neighbor, v|h])
;;   - keep this separate from the set of all neighbors, which is used by flooding
;; - pick an arbitrary neighbor-dir
;; - flood it
;; - remove all results from the set to try
;; - repeat with a new neighbor
;; 
;; This has to be a loop b/c we're modifying the collection we're picking things from as we iterate
(def neighbor-dirs
  [[1 0] [-1 0] [0 1] [0 -1]])

(defn sides [region]
  (let [edges (outside-neighbors region)]
    (loop [to-try (set (for [p edges, d [[1 0] [0 1]]] [p d]))
           results '()]
      (if-let [[pt _] (first to-try)]
        ;; In which directions are the adjacent region pts?
        ;; If left or right, this is a vertical side. If up or down, it's a horizontal side.
        ;; dirs will contain at most [0 1] and [1 0], and at least one of those
        (let [dirs (->> neighbor-dirs
                        (filter #(region (add-vec pt %)))  ;Don't check pts in the region
                        (map #(mapv abs %))
                        (map (comp vec reverse))  ;Apply the rule above
                        (filter (comp to-try #(vector pt %)))  ;Only try pt-dirs that haven't already been tried
                        set)
              found (for [d dirs]
                      (set (map #(vector % d)
                                (flood-side edges pt d))))]
          (recur (set/difference to-try (apply set/union #{[pt [0 1]] [pt [1 0]]} found))
                 (into results (map2 first found))))
        results))))

;; Problem: I'm trying to flood both directions but storing them separately. It can't tell the
;; difference between finding only 1 result in a direction and there being nothing to flood in that
;; direction, thus it's adding each tried point in twice: once tried horizontally and once tried
;; vertically.
;; Instead, to-try should just be points. In the loop, try both vertical and horizontal, keep the
;; one with more points in the result (each point should only actually be able to flood in one
;; direction or the other, we don't actually need the directions in the result)

;; Now I'm not double-adding sides that shouldn't be added, but I'm undercounting sides that
;; *should* be double-counted, i.e. neighbors in inside corners

;; Possible solution: don't just try both directions blindly, only try the direction of the side,
;; which can be determined by where around the neighbor there are members of the region set. If
;; there's a region pt left or right, it's a vertical side. If there's a region pt up or down, it's
;; a horizontal side. Both can be true

;; If I track which directions have been checked for each pt, then I get dups in results b/c if a pt
;; gets flooded into early and then has its other dir checked later, it'll check both dirs again and
;; flood back over where it's already done
;; If I don't track which directions have been checked for each pt, then if a pt gets flooded into
;; and removed early and its other dir was valid and not yet checked, it'll get skipped
;; Possible solution: track dirs, but when flooding in both directions, only flood in directions
;; still in to-try
;; Or said differently: when you pull a pt-dir out of to-try, also pull all the other dirs for that
;; pt

;; Freakin dang
;; AAAAA
;; A....
;; AAAA.
;; There's 2 horizontal sides that have the same pts, no way to differentiate them
;; Maybe keep track of the outside pt and the region pt? The border is between them

(defn pt2 [grid]
  (apply +
         (map (partial fork * count (comp count sides))
              (regions grid))))

(comment
  (let [grid (str/split-lines "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")]
    #_(pt2 grid)
    (map #(vector % (sides %)) (regions grid)))

  (sides #{[4 3] [2 2] [0 0] [1 0] [2 3] [4 2] [3 0] [4 1] [0 3] [2 4] [0 2] [2 0] [0 4] [2 1] [4 4] [0 1] [4 0]}))

;; AAAAA
;; ....A
;; AAAAA

;;  .....
;; .EEEEE.
;; .E.....
;; .EEEEE.
;; .E.....
;; .EEEEE.
;;  .....

;; Trying wall following instead
(defn rotate-cw [[y x]]
  [x (- y)])

(defn rotate-ccw [d]
  (mapv - (rotate-cw d)))

(defn upd
  "Like update, but gives the whole map to fn rather than just the value"
  [m k f]
  (assoc m k (f m)))

(defn step [region {:keys [inside outside dir side] :as args}]
  (let [[in out] (map #(add-vec dir %) [inside outside])]
    (cond
      (region out)
      (-> args
          ;; inside rotates 90° ccw around outside
          (assoc :inside (add-vec outside dir))
          (update :dir rotate-ccw)
          (update :results conj side)
          (upd :side #(list [(:inside %) outside])))

      (not (region in))
      (-> args
          ;; outside rotates 90° cw around inside
          (assoc :outside (add-vec inside dir))
          (update :dir rotate-cw)
          (update :results conj side)
          (upd :side #(list [inside (:outside %)])))

      :else
      (-> args
          (assoc :inside in :outside out)
          (update :side conj [in out])))))

(defn do-sides [inside-start outside-start rgn]
  (iterate (partial step rgn)
           {:inside inside-start
            :outside outside-start
            :dir (rotate-cw (add-vec outside-start (mapv - inside-start)))
            :side (list [inside-start outside-start])
            :results '()}))

(defn sides-from-start [[inside-start outside-start] rgn]
  (->> (do-sides inside-start outside-start rgn)
       (drop 1)
       (drop-while #(not (and (= (:inside %) inside-start)
                              (= (:outside %) outside-start))))
       first
       :results))

(comment
  (->> (do-sides [1 2] (add-vec [1 2] [-1 0])
                 #{[4 3]
                   [2 2]
                   [0 0]
                   [1 0]
                   [2 3]
                   [3 4]
                   [4 2]
                   [3 0]
                   [4 1]
                   [1 4]
                   [0 3]
                   [2 4]
                   [0 2]
                   [2 0]
                   [0 4]
                   [2 1]
                   [4 4]
                   [1 2]
                   [3 2]
                   [0 1]
                   [4 0]})
       (map (juxt :inside :outside))
       (take 5))
  ;; OOOOO
  ;; OXOXO
  ;; OOOOO
  ;; OXOXO
  ;; OOOOO

  ;; border-pts needs to take into account which outside point is being bordered with, just like we
  ;; need to do when tracking visited sides. In a region with holes in it, a border point can
  ;; participate in 2 different sides if the wall between the outside and the whole is only 1 thick
  )

(defn border-pts [rgn]
  (for [pt rgn
        n (neighbors pt)
        :when (not (rgn n))]
    [pt n]))

(defn sides2 [rgn]
  (loop [starts (set (border-pts rgn))
         sides '()]

    ;; We have to start at the beginning of a side. a start is [inside-pt outside-pt], so this will
    ;; prefer top-leftmost pts which will tend to be the beginning of horizontal sides, assuming
    ;; you're walking counter-clockwise
    (if (seq starts)
      (let [rslts (sides-from-start (spy (apply least starts)) rgn)]
        (recur (set/difference starts
                               (->> rslts (apply concat) set))
               (into sides rslts)))
      (count sides))))

(comment
  (sides2 #{[4 3] [2 2] [0 0] [1 0] [2 3] [4 2] [3 0] [4 1] [0 3] [2 4] [0 2] [2 0] [0 4] [2 1] [4 4] [0 1] [4 0]}))

(defn pt2-2 [grid]
  (let [rgns (regions grid)]
    (->> rgns
         (map (partial fork * count sides2))
         (apply +))))

(comment
  (pt2-2 (str/split-lines "AAAA
BBCD
BBCC
EEEC"))

  (pt2-2 (str/split-lines "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"))
  ;; Missing inside sides

  (pt2-2 (str/split-lines "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"))

  (sides2 (last (regions (str/split-lines "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"))))
  (do-sides [0 3] [1 3]
            (last (regions (str/split-lines "AAAAAA
                                                AAABBA
                                                AAABBA
                                                ABBAAA
                                                ABBAAA
                                                AAAAAA"))))
  ;; When starting at [[0 3] [1 3]], it wants to go left, which means it isn't at the start of a
  ;; side
  (add-vec [1 3] (mapv - [0 3])))

;; 1.
;; AAAAAA
;; AAABBA
;; AAABBA
;; ABBAAA
;; ABBAAA
;; AAAAAA

;; EEEEE
;; EXXXX
;; EEEEE
;; EXXXX
;; EEEEE

;; 2. At O at the pt above, we want the outside pt to stay the same
;; EEEEE
;; XXXXE
;; EEEEE
;; XXXXE
;; EEEOE

;; 3. At O and the pt above, we want the inside pt to stay the same
;; ......AA
;; AAAAAA.A
;; ....AAAA

;; These 2 conflict with each other. In both cases, an outside pt
;; goes inside the rgn, but in each we want different behavior.
;; If we prioritize outside pts over inside ones, it screws up these cases.
;; If we prioritize inside pts over outside ones, it screws up being able to start going in any
;; direction for the inside regions of 1.

;; Maybe some kind of corner detection?
;; In 3, it's a outside top-right corner b/c there's outside spaces above and right
;; i.e. outside space is empty (obv) and new space is empty
;; In 2, it's an inside bottom-right corner b/c there's only 1 outside space above
;; In 1, it's a inside top-right corner b/c ???

;; Corner counting: interesting idea
;;  .
;; AA.  AA  A.  .A ...
;; *A   A*  AA  AA AA.
;;                ...

;; Outside corners
;; .   .  A.  .A
;; A. .A  .    .  

;; Inside corners
;; A.  A   .A   A
;;  A  .A  A   A.

;; # corners = # sides

(comment
  ;; 2x2 window of matrix
  (partition 2 1 (range 10))
  (->> (vec (partitionv 4 (range 16)))
       ;; partition rows
       (map #(partitionv 2 1 %))
       ;; partition columns
       (partition 2 1)
       ;; transpose
       (mapcat #(apply map list %))))
;; 0 1 2 3
;; 4 5 6 7
;; 8 9 a b
;; c d e f

(defn window [mat]
  (->> mat
       ;; partition rows
       (map #(partitionv 2 1 %))
       ;; partition columns
       (partition 2 1)
       ;; transpose
       (mapcat #(apply map list %))))

(defn pad [mat]
  (let [xmax (+ 2 (count (first mat)))]
    (concat [(repeat xmax \.)]
            (map #(concat [\.] % [\.]) mat)
            [(repeat xmax \.)])))

(comment
  (pad [[\0 \1 \2]
        [\3 \4 \5]]))

;; Outside corners
;; .   .  A.  .A
;; A. .A  .    .  

;; Inside corners
;; A.  A   .A   A
;;  A  .A  A   A.

(defn base-outside-corner? [typ [[a _] [c d]]]
  (and (= c typ) (not= a typ) (not= d typ)))

(defn base-inside-corner? [typ [[a _] [c d]]]
  (and (= a typ) (= d typ) (not= c typ)))

(defn rotate [[[a b] [c d]]]
  [[c a]
   [d b]])

(defn corners [typ submat]
  (->> (take 4 (iterate rotate submat))
       (mapcat (juxt (partial base-outside-corner? typ)
                     (partial base-inside-corner? typ)))
       (filter identity)
       (take 2)
       count)
  #_(apply (some-fn (partial base-outside-corner? typ)
                    (partial base-inside-corner? typ))
           (take 4 (iterate rotate submat))))

(comment
  (corners 1 [[0 0]
              [1 1]]) 
  )

(defn count-corners [submat]
  (let [typs (set (flatten submat))]))

(comment
  (apply + (map (partial corners \A)
                (window (pad (str/split-lines "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")))))
  )
;; Something in here ain't working. (defn corners) seems to be correct, but this is coming up with
;; too many results

;; OOOOO
;; OXOXO
;; OOOOO
;; OXOXO
;; OOOOO

(comment
  (partition 7 (map {true 1, nil 0}
                    '(true
                      nil
                      nil
                      nil
                      nil
                      nil
                      true
                      nil
                      nil
                      nil
                      true
                      nil
                      true
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      true
                      nil
                      true
                      nil
                      true
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      true
                      nil
                      true
                      nil
                      nil
                      nil
                      true
                      nil
                      nil
                      nil
                      nil
                      nil
                      true))))
;; LEFT OFF: not correctly double-counting middle corner