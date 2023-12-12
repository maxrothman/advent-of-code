(ns clj2023.day5
  (:require [clj2023.util :refer [>>-> map2 spy spyf]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [helins.interval.map :as imap]
            [helins.interval.set :as iset]))

(comment
  (defmacro gen [d]
    (let [q (gensym)]
      `(l/run* [~q]
         ~@(for [r (eval d)] `(l/membero ~q ~r)))))

  (gen (for [start (range 3)] (vec (range start (+ 3 start)))))

  (defn f [] (let [v (l/lvar 'a)] [v (l/== 1 v)]))
  (let [[n v] (f)]
    (l/run* [b]
      v
      (l/== n b)))

  (l/run* [b]
    (l/== 1 (l/lvar 'a false))
    (l/== b (l/lvar 'a false)))

  (defn g [] (l/== 1 (l/lvar 'a false)))
  (l/run* [b]
    (g)
    (l/== b (l/lvar 'a false)))

  (l/defne t [x]
    (let [y 1]
      ([y])))

  (defn t [x]
    (let [y 1]
      (l/== x y)))

  (l/run* [a]
    (t a))

  (defn f [lvar]
    [(l/== lvar 1)
     (l/== lvar 2)])
  (l/run* [a]
    (l/or* (f a)))
  ;; => (1 2)
  (l/run* [a]
    (l/conde (f a)))
  ;; => Error printing return value (IllegalArgumentException) at clojure.core/-cache-protocol-fn (core_deftype.clj:584).
  ;;    No implementation of method: :ready? of protocol: #'clojure.core.logic.protocols/ISuspendedStream found for class: clojure.core.logic$_EQ__EQ_$fn__7943
  )
(defn cond-clauses
  "Necessary because core.logic's cond-clauses is private"
  [a]
  (fn [goals]
    `((~(first goals) ~a) ~@(rest goals))))

(defmacro conda
  "Version of conda that takes a coll of clauses rather than clauses as varargs.
   Necessary for creating programs with variable numbers of clauses. Copied directly
   from clojure.core.logic."
  [clauses]
  (let [a (gensym "a")]
    `(fn [~a]
       (l/ifa* ~@(map (cond-clauses a) clauses)))))

(defn map-range
  "Return a constraint mapping lvar-in to lvar-out
   from the src range to the dest range"
  [lvar-in lvar-out [dest-start src-start length]]
  (let [src-end (+ src-start (dec length))
        dest-end (+ dest-start (dec length))]
    (l/and*
     [(fd/in lvar-in (fd/interval src-start src-end))
      (fd/in lvar-out (fd/interval dest-start dest-end))
      (fd/eq (= lvar-out
                (+ (- lvar-in src-start)
                   dest-start)))])))

(defn invert-src [lvar [_ src-start length]]
  (l/or* [(fd/in lvar (fd/interval 0 (dec src-start)))
          (fd/in lvar (fd/interval (+ src-start length) Integer/MAX_VALUE))]))

(defn map-ranges
  "Return a constraint mapping lvar-in to lvar-out using one of
   the provided ranges if one matches, and with (== lvar-in lvar-out)
   if not"
  [lvar-in lvar-out ranges]
  (let [rs (mapv #(map-range lvar-in lvar-out %) ranges)
        inverse-rs (mapv #(invert-src lvar-in %) ranges)]
    (l/or*
     (conj rs
           (l/and* (conj inverse-rs
                         (l/== lvar-in lvar-out)))))))

(comment
  (l/run* [x]
    (conda
     [[(l/== x 1)]
      [(l/== x 2)]]))

  (l/run* [soil]
    (l/fresh [seed fertilizer water light temp humidity loc]
      (fd/in seed (fd/domain 98 99 #_[79 14 55 13]))
      (l/or*
       [(map-range seed soil 50 98 2)
        (l/== seed soil)])))
  ;; => (98 99 50 51)
  ;; Use conda to stop using options after one has matched
  (l/run* [soil]
    (l/fresh [seed fertilizer water light temp humidity loc]
      (fd/in seed (fd/domain 98 99 100 101 #_[79 14 55 13]))
      (l/conda
       [(map-range seed soil [50 98 2])]
       [(l/== seed soil)])))
  ;; Except that there's no equivalent of or* for conda. Maybe I can rule out the default match some
  ;; other way? Also if any members of the domain match a clause, it stops matching all other
  ;; clauses even if some of the domain members only match further clauses, which is no good

  (l/run* [soil]
    (l/fresh [seed fertilizer water light temp humidity loc]
      (fd/in seed (fd/domain 98 99 100 101 #_[79 14 55 13]))
      (l/conde
       [(map-range seed soil [50 98 2])]
       [(l/or* [(fd/in seed (fd/interval 0 97))
                (fd/in seed (fd/interval 100 Integer/MAX_VALUE))])
        (l/== seed soil)])))

  (l/run* [soil]
    (l/fresh [seed]
      (fd/in seed (fd/domain 79 14 55 13))
      (map-ranges seed soil [[50 98 2] [52 50 48]])))
  ;; => (57 81 13 14)
  ;; Yay!
  )
(def test-raw
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse [raw]
  (let [[start & maps] (->> (str/split raw #"\n\n")
                            (map str/split-lines)
                            (map2 #(re-seq #"\d+" %))
                            (map2 #(map parse-long %)))]
    [(first start)
     (map #(filter seq %) maps)]))

(comment
  @(def test-data (parse test-raw)))

(defn pt1 [data]
  (let [[start maps] (parse data)
        {:keys [lvars constraint]}
        (reduce (fn [{:keys [lvars constraint]} m]
                  (let [lv (l/lvar)]
                    {:lvars (conj lvars lv)
                     :constraint (l/composeg constraint
                                             (map-ranges (peek lvars)
                                                         lv
                                                         m))}))
                {:lvars [(l/lvar)], :constraint l/succeed}
                maps)]
    (l/run* [out]
      (l/fresh [init]
        (fd/in init (apply fd/domain start))
        (l/== init (first lvars))
        constraint
        (l/== out (peek lvars))))))

(comment
  (pt1 test-raw)
  ;; => (86 82 43 35)

  (pt1 (slurp (io/resource "day5.txt")))
  ;; While it may work, it's too slow to complete the puzzle :(
  )
;; Trying again with interval maps

(comment
  (parse test-raw))

(defn ->mapper [old-start new-start]
  (fn mapp [old]
    (+ (- old old-start)
       new-start)))

(defn start-len->interval [start len]
  [start (+ start len)])

(defn ->entry [[dest-start src-start length]]
  (conj (start-len->interval src-start length)
        (->mapper src-start dest-start)))

(defn ->intervals [raws]
  (reduce (partial apply imap/mark)
          imap/empty
          (map ->entry raws)))

(defn pt1-fast [raw]
  (let [[start raw-maps] (parse raw)
        maps (map ->intervals raw-maps)]
    (map #(reduce (fn [v m]
                    (if-let [[mapper] (seq (get m v))]
                      (mapper v)
                      v))
                  %
                  maps)
         start)))

(comment
  (apply min (pt1-fast test-raw))
  ;; => 35

  (apply min (pt1-fast (slurp (io/resource "day5.txt"))))
  ;; => 51580674
  )

(comment
  (-> imap/empty (imap/mark 5 10 :a) (imap/mark 6 8 :b)
      (subseq >= 4 <= 9))
  ;; => ([[5 6] #{:a}] [[6 8] #{:b :a}] [[8 10] #{:a}])

  ;; If the search range isn't fully mapped, split it up
  (let [[s e] [4 15]
        rs '([[5 6] #{1}] [[7 8] #{1 2}] [[8 10] #{1}])
        rs-start (-> rs first first first)
        rs-end (-> rs last first second)]
    (cond-> rs
      (not= rs-start s) (conj [[s rs-start] #{:TODO}])
      (not= rs-end e) (conj [[rs-end e] #{:TODO}])))
  ;; This misses gaps in the result range

  (let [q [4 15]
        rs '([[5 6] #{1}] [[7 8] #{1 2}] [[8 10] #{1}])]
    (reduce (partial apply iset/erase)
            (->iset [q])
            (map first rs)))
  ;; This catches all the gaps
  ;; Keep these ranges into the next mapping 

  ;; Don't need a mapper anymore, can map intervals directly to intervals

  (let [[raw-starts raw-maps] (parse test-raw)
        starts (map (partial apply start-len->interval)
                    (partition 2 raw-starts))
        maps (map ->intervals raw-maps)]
    #_(subseq (first maps) >= 50 <= 103)
    #_starts
    ;; NB: < and > don't seem to work correctly, only >= and <=
    ;; Because < and > require the intervals to be fully covered by a segment to match, >= and <=
    ;; allow segments to intersect
    )
  (-> iset/empty (iset/mark 55 68) (iset/mark 79 93))
  (-> iset/empty (iset/mark 0 5) (iset/mark 10 15))
  ;; Bug: lowest interval must be inserted first https://github.com/helins/interval.cljc/issues/2

  (subseq (-> imap/empty (imap/mark 0 10 :a))
          >= 5 <= 7)
  ;; Crap, subseq returns ranges that are too big too, so I need to snip them
  )
(defn intersection-wrong [ivls ivl-map]
  (let [matches (mapcat #(subseq ivl-map >= (first %) <= (second %))
                        ivls)
        gaps (reduce (partial apply iset/erase)
                     (->iset (map first matches))
                     (keys ivl-map))]
    gaps))

(defn ->imap [map-entries]
  (reduce (fn [m [[s e] v]] (imap/mark m s e v))
          imap/empty
          map-entries))

(defn ->iset [ivls]
  (reduce (partial apply iset/mark)
          iset/empty
          (sort-by first ivls)))

(defn erase
  "Interval set/map erase that's polymorphic over sets and maps.
   If a non-nil value is provided and coll is a map, erases only that value at
   the specified interval."
  [coll itm]
  ;; Set entries look like [s e] while map entries look like [[s e] #{v}]. We can tolerate v==nil
  (let [[s e v] (if (vector? (first itm))
                  (mapcat identity itm)
                  itm)]
    (if (map? coll)
      (if (nil? v)
       ;; imap/erase only lets us erase one value at a time, so we have to erase them all
       ;; individually
        (->> (subseq coll >= s <= e)
             (mapcat second)
             (into #{})
             (reduce #(imap/erase %1 s e %2) coll))
        (imap/erase coll s e v))
      (iset/erase coll s e))))

(comment
  (erase (->imap [[[0 4] :a] [[2 6] :b]]) [[1 4] #{:a}])
  (erase (->iset [[0 4] [2 6]]) [[1 4] #{:a}])
  (erase (->iset [[0 4] [2 6]]) [1 4])
  (erase (->imap [[[0 4] :a] [[2 6] :b]]) [1 4]))

(defn difference [a b]
  (reduce erase a b))

(defn intersection [a b]
  (difference a (difference a b)))

(comment
  (difference (->imap [[[0 4] :a] [[2 6] :b]]) (->iset [[2 4]]))
  (difference (->imap [[[0 4] :a] [[2 6] :b]]) (->imap [[[2 5] :a]]))

  (intersection (->imap [[[0 4] :a] [[2 6] :b]]) (->iset [[2 4]]))
  (intersection (->imap [[[0 4] :a] [[2 6] :b]]) (->imap [[[2 5] :a]])))

(defn ->entry2
  "Given:
   - a source interval I defined by (src-start, src-start + length)
   - a destination interval J defined by (dest-start, dest-start + length)
   Returns a map entry of (I, N) such that for any i in I, i + N = the
   corresponding element of J"
  [[dest-start src-start length]]
  [(start-len->interval src-start length)
   (- dest-start src-start)])

(comment
  (let [[starts maps] (parse test-raw)]
    (mapv ->imap (map2 ->entry2 maps))))

(defn ivl+
  "Add to both bounds of ivl by n"
  [ivl n]
  (mapv (partial + n) ivl))

(defn step
  "Given an iset and an imap of adjustments, map ivls to new ivls using
   the adjustments where matches are found in imap, and including the unmapped
   intervals unchanged where not"
  [ivls imap]
  (->iset
   (into
    (difference ivls imap)
    (map (fn [[ivl v]] (ivl+ ivl (first (seq v))))
         (intersection imap ivls)))))

(comment
  (step (->iset [[0 3] [5 7]]) (->imap [[[2 3] 5] [[3 6] 10]])))

(defn pt2 [raw]
  (let [[raw-starts raw-maps] (parse raw)
        starts (->iset (map (partial apply start-len->interval) (partition 2 raw-starts)))
        maps (->> (map2 ->entry2 raw-maps) (map ->imap))]
    (reduce step starts maps)))

(comment
  (->> (pt2 test-raw)
       (mapcat identity)
       (apply min))
  ;; => 46

  (->> (pt2 (slurp (io/resource "day5.txt")))
       (mapcat identity)
       (apply min))
  )

;; Goal: map ranges in src through map to dest
;; Need:
;; - segments of mapped ranges present in src, their corresponding modifiers
;; - unmatched segments
(comment
  (let [[raw-starts raw-maps] (parse test-raw)
        starts (->iset (map (partial apply start-len->interval) (partition 2 raw-starts)))
        maps (->> (map2 ->entry2 raw-maps) (map ->imap))
        mapped (intersection (first maps) starts)
        unmapped (difference starts (first maps))]
    (->iset
     (into (map (fn [[ivl v]] (ivl+ ivl (first (seq v)))) mapped)
           unmapped))))

(defn map-ivl [src imap]
  (->> (mapcat (fn [[s e]] (subseq imap >= s <= e)) src)  ;ivls in imap matching src
       (map #(update % 1 (comp first seq)))  ;Get rid of the sets around the vals so we can turn it back into an imap
       ->imap
       (>>-> difference src)))

(comment
  (map-ivl [[2 4] [6 8]]
           (->imap [[[0 5] 7] [[5 10] -3]])))

(comment
  (->iset [[10 15] [0 5]])
  (intersection [[5 10]] (-> imap/empty (imap/mark 0 20 :a)))

  (let [[raw-starts raw-maps] (parse test-raw)
        starts (->iset (map (partial apply start-len->interval) (partition 2 raw-starts)))
        maps (map ->intervals2 raw-maps)]
    (intersection starts (first maps))
    #_(reduce (fn [ivls m]
                ()
                #_(if-let [[mapper] (get m v)]
                    (mapper v)
                    v))
              starts
              maps)))

(comment
  ;; Testing dthume's interval treeset
  (it/it-difference (into (it/interval-treeset) [[5 10]])
                    (into (it/interval-treeset) [[5 6] [9 10]]))
  ;; Nope
  )