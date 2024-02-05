(ns clj2023.day19
  (:require [clj2023.util :refer [spyf until]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [helins.interval.set :as iset]))

(defn parse-wflw [line]
  (let [[nme rules-raw] (rest (re-matches #"(\w+)\{([^}]+)\}" line))
        splitted (str/split rules-raw #",")
        [butlst [lst]] (split-at (dec (count splitted)) splitted)
        rules (for [r butlst
                    :let [[field op n nxt] (rest (re-matches #"(\w+)([<>])(\w+):(\w+)" r))]]
                #(when ((case op "<" < ">" >)
                        ((keyword field) %)
                        (parse-long n))
                   nxt))]
    [nme (concat rules (list (constantly lst)))]))

(defn parse-itm [line]
  (->> (re-seq #"([xmas])=(\d+)" line)
       (map (comp vec rest))
       (map #(update % 1 parse-long))
       (map #(update % 0 keyword))
       (into {})))

(defn parse [raw]
  (let [[wflws itms] (str/split raw #"\n\n")]
    [(into {} (map parse-wflw (str/split-lines wflws)))
     (map parse-itm (str/split-lines itms))]))

(comment
  @(def test-data
     (parse "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"))

  ((apply some-fn (parse-wflw "px{a<2006:qkq,m>2090:A,rfg}")) {:a 2006 :m 2090}))

(defn go-1 [wflws itm]
  (until #{"A" "R"}
         #((apply some-fn (get wflws %)) itm)
         "in"))

(comment
  (go-1 (first test-data) (nth (second test-data) 4)))

(defn go [wlfws itms]
  (filter #(#{"A"} (go-1 wlfws %)) itms))

(comment
  (->> (apply go test-data)
       (mapcat vals)
       (apply +))
  
  (->> (apply go (parse (slurp (io/resource "day19.txt"))))
       (mapcat vals)
       (apply +))
  )

(defn parse-wflw2 [line]
  (let [[nme rules-raw] (rest (re-matches #"(\w+)\{([^}]+)\}" line))
        splitted (str/split rules-raw #",")
        [butlst [lst]] (split-at (dec (count splitted)) splitted)
        rules (for [r butlst
                    :let [[field op n dest] (rest (re-matches #"(\w+)([<>])(\w+):(\w+)" r))]]
                {:field (keyword field), :op op, :n (parse-long n), :dest dest})]
    [nme (concat rules (list {:op :default, :dest lst}))]))

(defn parse2 [raw]
  (let [[wflws _] (str/split raw #"\n\n")]
    (into {} (map parse-wflw2 (str/split-lines wflws)))))

(defn union [set1 set2]
  (reduce (partial apply iset/mark) set1 set2))

(def rangemax 4000)

(defn go-1' [state {:keys [field op n dest]}]
  (let [dest-k (if (#{"A" "R"} dest) :finished :processing)
        mark-safe (fnil iset/mark iset/empty)]
    (case op
      ">" (-> state
              (update-in [:itm field] iset/erase (inc n) rangemax)
              (update-in [dest-k dest field] mark-safe (inc n) rangemax))
      "<" (-> state
              (update-in [:itm field] iset/erase 0 (dec n))
              (update-in [dest-k dest field] mark-safe 0 (dec n)))
      :default (-> (assoc state :itm nil)
                   (update-in [dest-k dest]
                              (partial merge-with union) (:itm state))))))

;; This isn't quite right
;; The goal is "how many items in A", where "item" is a combination of x m a s
;; A and R need to account for x m a s
;; Putting ranges under x m a s is a kind of "and" operation
;; e.g. {x [8 9] m [0 1] a [2 3] s [5 6]} is this set of items:
;; {x 8 m 0 a 2 s 5}
;; {x 9 m 0 a 2 s 5}
;; {x 8 m 1 a 2 s 5}
;; {x 9 m 1 a 2 s 5}
;; ...
;; Is it safe to merge items when adding them to a name?
;; {x [9 10] m [5 6]}, {x [1 2] m [3 4]}
;; Each 4 itms
;; Merged implies {x 1 m 5} is accepted, which is false
;; So no?

;; Ok, I think the approach is that :finished and :processing should be
;; {name {x|m|a|s -> iset}}
;; NOT SURE ^
;; No, def not. Either gotta think of it as a 4-d coordinate space and use something like an R-tree,
;; or have :finished be {A|R (list {x|m|a|s -> iset})}, and :processing be {name (list {x|m|a|s ->
;; iset}})
;; ITEMS CANNOT OVERLAP
;; Proof:
;; - For 2 items to intersect, all of their dimensions must intersect. If any dimension does
;;   not intersect, the items do not intersect.
;; - All items are split from the same initial range, and there's only one entrypoint. For two items
;;   to be split, they must've at one point had a non-intersecting dimension
;; ∴ No items intersect
;; That means all the intersection stuff is unnecessary, just get the list of ranges, determine
;; each's size, and add 'em up

;; Do the entire processing map at once. In that case does it need to be a map?
;; Finished might not need to be a map either, I can just drop the Rs
(def go-init
  {:finished {"A" iset/empty, "R" iset/empty}
   :processing {}
   :itm (zipmap [:x :m :a :s] (repeat (iset/mark iset/empty 0 rangemax)))})

(comment
  @(def test-data
     (parse2 "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"))

  (reduce go-1
          go-init
          (test-data "in"))
  )
