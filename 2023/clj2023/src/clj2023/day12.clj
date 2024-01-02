(ns clj2023.day12
  (:require [clj2023.util :refer [>>-> progress resource-lines]]
            [clojure.core.logic :as l]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.string :as str]))

(comment

;; Parse #.#.### 1,1,3 to
  [1 1 3] [1 1 3]

;; Parse ???.### 1,1,3 to
  '([? 3] [o 1] [x 3]) '(1 o+ 1 o+ 3)

;; Which reduces to
  '(? ? ?) '(x o x)

;; So it's kind of like regex, I can take the range lengths and say they're separated by 1 or more
;; os

;; The RHS could be
  (concato [x] os [x] os [x x x]))
;; Where os is
(defn repeato
  ([q c] (repeato q c (list c) 10))
  ([q c s]
   (l/fresh [s']
     (l/conso c s s')
     (l/conde
       [(l/== q s)]
       [(repeato q c s')]))))

(comment
  (l/run 5 [q]
    (repeato q 'o)))

(defn concato* [q ss]
  (l/matcha [ss]
    ([[s]] (l/== q s))
    ([[s . ss']] (l/fresh [q']
                   (concato* q' ss')
                   (l/appendo s q' q)))))

;; and concato is defined recursively in terms of appendo
(defn concato [q & ss]
  (concato* q ss))

(comment
  (l/run* [q]
    (concato q [1 2] [3 4] [5 6]))
  (l/run* [q]
    (concato q [1 2] [3 4]))
  (l/run* [q]
    (concato q [1 2]))
  (l/run* [q]
    (concato q)))

(defn interposeo
  "Relational version of interpose where the separator is (apply goal q args),
   and q is always fresh"
  [q s goal & args]
  (l/matcha [s]
    ([[a]] (l/== q (list a)))
    ([[a . ss]]
     (l/fresh [q' q'']
       (apply goal q' args)
       (apply interposeo q'' ss goal args)
       (l/== q
             (l/llist a q' q''))))))

(comment
  (l/run* [q]
    (interposeo q [1 2 3] concato [0] [0])))

(comment
  ;; Let's try again with ???.### 1,1,3
  (l/run 1 [a b c]
    (l/fresh [q' q'']
      (interposeo q' [['x] ['x] ['x 'x 'x]] repeato 'o)
      (concato* q'' q')
      (l/== [a b c 'o 'x 'x 'x]
            q'')))

  ;; And with .??..??...?##. 1,1,3
  (l/run 3 [a b c d e]
    (l/fresh [q' q'']
      (interposeo q' [['x] ['x] ['x 'x 'x]] repeato 'o)
      (concato* q'' q')
      (l/== [a b 'o 'o c d 'o 'o 'o e 'x 'x]
            q'')))
  ;; It seems to hang here, with run's limit > 2, as does it when the first example's limit is increased beyond 1.
  ;; Something seems fucky here, maybe I built it wrong?

  ;; It works for run limit = 2, I wonder if it's searching by expanding one of the repeats to
  ;; infinity before trying to expand any of the others.
  ;; I wonder if a more performant formulation would go the other way: parse the sequence to the
  ;; spans and check if they're equal. Then you can't expand the sequence to infinity 
  )

(defn spans [c s]
  (->> (partition-by identity s)
       (filter #(-> % first (= c)))
       (map count)))

(comment
  (= (spans 'x '(x o x o x x x)) [1 1 3])

  (l/run* [a b c]
    (l/fresh [q]
      (l/membero a '(x o))
      (l/membero b '(x o))
      (l/membero c '(x o))
      (l/== q [a b c 'o 'x 'x 'x])
      (l/pred q #(= [1 1 3] (spans 'x %)))))
  ;; This works!
  
  (l/run* [a b c d e]
    (l/fresh [q]
      (l/membero a '(x o))
      (l/membero b '(x o))
      (l/membero c '(x o))
      (l/membero d '(x o))
      (l/membero e '(x o))
      (l/== q ['o a b 'o 'o c d 'o 'o 'o e 'x 'x 'o])
      (l/pred q #(= [1 1 3] (spans 'x %)))))
  ;; This works too!
  
  (-> (l/run* [a b c d e f g h i]
        (l/fresh [q]
          (l/membero a '(x o))
          (l/membero b '(x o))
          (l/membero c '(x o))
          (l/membero d '(x o))
          (l/membero e '(x o))
          (l/membero f '(x o))
          (l/membero g '(x o))
          (l/membero h '(x o))
          (l/membero i '(x o))
          (l/== q [a 'x 'x 'x b c d e f g h i])
          (l/pred q #(= [3 2 1] (spans 'x %)))))
      count)
  ;; This too! I think this is a viable approach 
  ;; But this is basically just a big cartisian product/for loop, I don't think it's doing anything
  ;; clever. It might be less hassle to just write the loop myself or use math.combinatorics/cartisian-product
  

  (str/split "ccccababbabb" #"(?=a)|(?<=a)")
  ;; => ["cccc" "a" "b" "a" "bb" "a" "bb"]
  
  (->> (count (re-seq #"\?" ".??..??...?##."))
       (>>-> repeat [\. \#])
       (apply cartesian-product)
       (map #(replace-with ".??..??...?##." #{\?} %))
       (map (partial spans \#))
       (filter #{[1 1 3]})
       count)
  )

(defn replace-with
  "Replace elements of s1 with successive elements of s2"
  [s1 pred s2]
  (if-let [[x & rst1] s1]
    (if (pred x)
      (cons (first s2) (replace-with rst1 pred (rest s2)))
      (cons x (replace-with rst1 pred s2)))
    s1))

(defn row [[l r]]
  (->> (count (re-seq #"\?" l))
       (>>-> repeat [\. \#])
       (apply cartesian-product)
       (map #(replace-with l #{\?} %))
       (map (partial spans \#))
       (filter #{r})
       count))

(defn pt1 [data]
  (apply + (progress (map row data))))

(def test-data
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn parse-1 [raw]
  (-> (str/split raw #" ")
      (update 1 (comp (partial mapv parse-long) #(str/split % #",")))))

(comment
  (->> test-data
       str/split-lines
       (map parse-1)
       pt1)
  
  (time (resource-lines "day12.txt" (comp pt1 (partial map parse-1))))
  ;; 2.2792 minutes
  )

