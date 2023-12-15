(ns clj2023.day5
  (:require [clj2023.util :refer [>>-> map2 spy spyf]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [helins.interval.map :as imap]
            [helins.interval.set :as iset]))

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
  ;; => 99751240

  )
