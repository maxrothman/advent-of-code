(ns advent-of-code.day12
  (:require [clojure.string :as str]
            [advent-of-code.util :as util]
            [clojure.data :as data]))

(def test1
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def test1-expected
  #{["start" "A" "b" "A" "c" "A" "end"]
    ["start" "A" "b" "A" "end"]
    ["start" "A" "b" "end"]
    ["start" "A" "c" "A" "b" "A" "end"]
    ["start" "A" "c" "A" "b" "end"]
    ["start" "A" "c" "A" "end"]
    ["start" "A" "end"]
    ["start" "b" "A" "c" "A" "end"]
    ["start" "b" "A" "end"]
    ["start" "b" "end"]})

(def test2 (parse "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"))

(def test2-expected
  #{["start" "HN" "dc" "HN" "end"]
    ["start" "HN" "dc" "HN" "kj" "HN" "end"]
    ["start" "HN" "dc" "end"]
    ["start" "HN" "dc" "kj" "HN" "end"]
    ["start" "HN" "end"]
    ["start" "HN" "kj" "HN" "dc" "HN" "end"]
    ["start" "HN" "kj" "HN" "dc" "end"]
    ["start" "HN" "kj" "HN" "end"]
    ["start" "HN" "kj" "dc" "HN" "end"]
    ["start" "HN" "kj" "dc" "end"]
    ["start" "dc" "HN" "end"]
    ["start" "dc" "HN" "kj" "HN" "end"]
    ["start" "dc" "end"]
    ["start" "dc" "kj" "HN" "end"]
    ["start" "kj" "HN" "dc" "HN" "end"]
    ["start" "kj" "HN" "dc" "end"]
    ["start" "kj" "HN" "end"]
    ["start" "kj" "dc" "HN" "end"]
    ["start" "kj" "dc" "end"]})

(def test3 (parse "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"))

(defn parse [d]
  (->> d
       (#(str/split % #"\n"))
       (map #(str/split % #"-"))
       (util/fork #(merge-with into %1 %2)
                  #(util/group-with first second %)
                  #(util/group-with second first %))))

(def test1-parsed (parse test1))

(defn little? [x] (util/fork = str/lower-case identity x))
(defn end? [x] (= x "end"))
(defn descendant-paths [p graph]
  (let [no-return (set (filter little? p))]
    (->> (graph (first p))    ;Descendents
         (remove no-return)   ;Only big caves can be returned to
         (map #(conj p %)))))
(comment
  (descendant-paths '("A" "start") test1-parsed)
  ;; => (("c" "A" "start")
  ;;     ("b" "A" "start")
  ;;     ("end" "A" "start"))

  (descendant-paths '("A" "c" "A" "start") test1-parsed)
  ;; => (("b" "A" "c" "A" "start")
  ;;     ("end" "A" "c" "A" "start"))
  )

(defn search-step [{[[node :as p] & ps] :todo,
                    :keys [paths graph]}]
  (if (end? node)
    {:todo ps
     :paths (conj paths p)
     :graph graph}
    {:todo (into ps (descendant-paths p graph))
     :paths paths
     :graph graph}))

(defn search [graph]
  (->> (util/until (comp empty? :todo)
                   search-step
                   {:todo '(("start")) :paths #{} :graph graph})
       :paths
       (map reverse)
       set))

(comment
  (->> (iterate search-step
                {:todo '(("start")) :paths #{} :graph test1-parsed})
       (take-while (comp seq :todo))
       (map :todo))


  (= (search test1-parsed)
     test1-expected)
  ;; => true

  (= (search test2) test2-expected)
  ;; => true

  (= 226 (count (search test3)))
  ;; => true
  )

(comment
  ;; Part 1
  (-> (slurp "resources/day12/input.txt")
      parse
      search
      count)
  ;; => 3421
  )

;; Part 2

(defn descendant-paths-2 [p graph]
  (let [no-more-littles (->> (frequencies p)
                             (filter (comp little? first))
                             (filter (comp #(< 1 %) second))
                             seq)
        no-return (into #{"start"}
                        (if no-more-littles
                          (filter little? p)
                          #{}))]
    (->> (graph (first p))    ;Descendents
         (remove no-return)   ;Only big caves can be returned to
         (map #(conj p %)))))

(defn search-step [{[[node :as p] & ps] :todo,
                    :keys [paths graph]}]
  (if (end? node)
    {:todo ps
     :paths (conj paths p)
     :graph graph}
    {:todo (into ps (descendant-paths-2 p graph))
     :paths paths
     :graph graph}))

(defn search-2 [graph]
  (->> (util/until (comp empty? :todo)
                   search-step-2
                   {:todo '(("start")) :paths #{} :graph graph})
       :paths
       (map reverse)
       set))

(def test1-expected-2
  #{["start" "A" "b" "A" "b" "A" "c" "A" "end"]
    ["start" "A" "b" "A" "b" "A" "end"]
    ["start" "A" "b" "A" "b" "end"]
    ["start" "A" "b" "A" "c" "A" "b" "A" "end"]
    ["start" "A" "b" "A" "c" "A" "b" "end"]
    ["start" "A" "b" "A" "c" "A" "c" "A" "end"]
    ["start" "A" "b" "A" "c" "A" "end"]
    ["start" "A" "b" "A" "end"]
    ["start" "A" "b" "d" "b" "A" "c" "A" "end"]
    ["start" "A" "b" "d" "b" "A" "end"]
    ["start" "A" "b" "d" "b" "end"]
    ["start" "A" "b" "end"]
    ["start" "A" "c" "A" "b" "A" "b" "A" "end"]
    ["start" "A" "c" "A" "b" "A" "b" "end"]
    ["start" "A" "c" "A" "b" "A" "c" "A" "end"]
    ["start" "A" "c" "A" "b" "A" "end"]
    ["start" "A" "c" "A" "b" "d" "b" "A" "end"]
    ["start" "A" "c" "A" "b" "d" "b" "end"]
    ["start" "A" "c" "A" "b" "end"]
    ["start" "A" "c" "A" "c" "A" "b" "A" "end"]
    ["start" "A" "c" "A" "c" "A" "b" "end"]
    ["start" "A" "c" "A" "c" "A" "end"]
    ["start" "A" "c" "A" "end"]
    ["start" "A" "end"]
    ["start" "b" "A" "b" "A" "c" "A" "end"]
    ["start" "b" "A" "b" "A" "end"]
    ["start" "b" "A" "b" "end"]
    ["start" "b" "A" "c" "A" "b" "A" "end"]
    ["start" "b" "A" "c" "A" "b" "end"]
    ["start" "b" "A" "c" "A" "c" "A" "end"]
    ["start" "b" "A" "c" "A" "end"]
    ["start" "b" "A" "end"]
    ["start" "b" "d" "b" "A" "c" "A" "end"]
    ["start" "b" "d" "b" "A" "end"]
    ["start" "b" "d" "b" "end"]
    ["start" "b" "end"]})

(comment
  (= test1-expected-2
     (search-2 test1-parsed))
  ;; => true

  (-> (slurp "resources/day12/input.txt")
      parse
      search-2
      count)
  ;; => 84870
  )