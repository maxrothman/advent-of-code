(ns clj2023.day19
  (:require [clj2023.util :refer [spy until]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [helins.interval.set :as iset]
            [clj2023.interval-set :as iset2]))

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

(def rangemin 1)
(def rangemax 4000)

(defn do-1-step [{:keys [itm] :as state} {:keys [field op n dest]}]
  (let [route (fn [state dest itm']
                (case dest
                  "A" (update state :accepted conj itm')
                  "R" state  ;drop it
                  (update state :processing conj [dest itm'])))]
    (case op
      ">" (-> state
              (update-in [:itm field] iset2/erase (inc n) rangemax)
              (route dest (update itm field iset2/erase rangemin n)))
      "<" (-> state
              (update-in [:itm field] iset2/erase rangemin (dec n))
              (route dest (update itm field iset2/erase n rangemax)))
      (-> (dissoc state :itm)
          (route dest itm)))))

(comment
  (iset/erase (iset/mark iset/empty 2876 2876) 1351 4000)
  (iset/erase (iset/mark iset/empty 1 1) 0 3)
  (imap/erase (imap/mark imap/empty 1 1 :hi) 0 3 :hi)
  
  (first (disj (itree/interval-set [[1 2]]) [1 2]))
  (first (itree/interval-set)) 
  (conj (itree/interval-set [[1 2]]) [1 3])
  )
;; Neither library does the right thing. itree doesn't merge sets and can't conj/disj partial
;; intervals. iset has bugs where adjacent intervals are merged if they're added in the wrong order,
;; and 0-length intervals can't be removed.
;; Could wrap
;; https://guava.dev/releases/23.0/api/docs/com/google/common/collect/ImmutableRangeSet.html
;; But for them "immutable" means "can't be modified at all no exceptions" so there's no mark/erase
;; ops. Could wrap their mutable TreeRangeSet, if it actually works the way I need it to. Will my
;; code work with mutable sets?



(defn do-1-itm [wflws state [wflw itm]]
  (reduce do-1-step (assoc state :itm itm) (wflws wflw)))

(defn do-all [wflws]
  (until (comp empty? :processing) 
         #(reduce (partial do-1-itm wflws)
                  (assoc % :processing '())
                  (:processing %))
         {:accepted '() ;(list iset)
          :processing
          (list ["in" (zipmap [:x :m :a :s]
                              (repeat (iset2/range-set [rangemin rangemax])))])}))

(defn combos [itm]
  (->> (vals itm)
       (map #(->> (iset2/to-seq %)
                  (map (comp inc abs (partial apply -)))
                  (apply +))) 
       (apply *)))

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

  (->> (do-all test-data)
       :accepted
       (map combos)
       (apply +))
  ;; => 167409079868000
  ;; Correct! Got a different answer with iset, probably due to a bug
  
  (->> (parse2 (slurp (io/resource "day19.txt")))
       do-all
       :accepted
       (map combos)
       (apply +))
  )
