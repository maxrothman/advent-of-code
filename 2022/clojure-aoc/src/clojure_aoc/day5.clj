(ns clojure-aoc.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn nths
  "Return the specified indices of coll
   
   indices is expected to be sorted, indices not in the coll are ignored."
  [coll indices]
  (map #(nth coll %)
       (take-while #(< % (count coll)) indices)))

(def debug (atom nil))

(defn parse [input]
  (let [[stacks-raw commands-raw] (str/split input #"\n\n")
        [stack-data stack-name-data] ((juxt butlast last) (str/split-lines stacks-raw))
        stack-names (str/replace stack-name-data #" " "")
        stacks (->> stack-data
                    (map #(nths % (range 1 9999 4)))        ;Pick out letter values
                    (apply map vector)                      ;Transpose
                    (map #(remove (partial = \space) %))    ;Remove spaces
                    (zipmap stack-names))

        commands (->> (str/split-lines commands-raw)
                      ;; Extract important bits of text
                      (map #(->> (re-matches #"move ([^ ]+) from ([^ ]+) to ([^ ]+)" %)
                                 rest))
                      ;; Expand repeated commands
                      (mapcat #(repeat (parse-long (str (first %)))
                                       (rest %)))
                      ;; The stacks map is indexed on chars, so turn the strings into chars. There
                      ;; are at most 9 stacks, so no risk of double-digits
                      (map #(map first %)))]
    [stacks commands]))
(comment
  (parse input)
  )

(defn move [stacks [from to]]
  (if-let [itm (-> stacks (get from) first)]
    (-> stacks
        (update from rest)
        (update to conj itm))
    (throw (ex-info "Can't take from stack"
                    {:command [from to]
                     :from (get stacks from)
                     :to (get stacks to)}))))
(comment
  (move {\2 '(1 2 3) \1 '(3 2 1)} [\1 \2])
  (move {\2 '(1 2 3) \1 '()} [\1 \2])

  (let [[stacks commands] (parse input)]
    (reductions move stacks commands)) 
  )

(defn part1 [input]
  (->> input
       parse
       (apply reduce move)
       (map (comp first second))
       str/join))
(comment
  (part1 input)

  (part1 (slurp (io/resource "day5.txt")))
  (parse (slurp (io/resource "day5.txt")))
  (->> (slurp (io/resource "day5.txt"))
       parse
       (apply reductions move)
       (map vector (-> (slurp (io/resource "day5.txt"))
                       parse
                       second))
       (take 10))
  (-> (slurp (io/resource "day5.txt"))
      parse
      second
      (nth 150))

  ;; Something bad is happening at iteration 150. What command does that relate to? Maybe there's
  ;; something wrong with parsing.
  (first (parse (slurp (io/resource "day5.txt"))))
  (-> @debug
      (update 0 str/split-lines)
      (#(apply map vector %)))
  (re-matches #"move ([^ ]+) from ([^ ]+) to ([^ ]+)" "move 10 from 1 to 5")

  ;; Works!
  (part1 (slurp (io/resource "day5.txt")))
  )

;; Part 2

(defn parse2 [input]
  (let [[stacks-raw commands-raw] (str/split input #"\n\n")
        [stack-data stack-name-data] ((juxt butlast last) (str/split-lines stacks-raw))
        stack-names (str/replace stack-name-data #" " "")
        stacks (->> stack-data
                    (map #(nths % (range 1 9999 4)))        ;Pick out letter values
                    (apply map vector)                      ;Transpose
                    (map #(remove (partial = \space) %))    ;Remove spaces
                    (zipmap stack-names))

        commands (->> (str/split-lines commands-raw)
                      ;; Extract important bits of text
                      (map #(->> (re-matches #"move ([^ ]+) from ([^ ]+) to ([^ ]+)" %)
                                 rest))
                      ;; The stacks map is indexed on chars, so turn the strings into chars. There
                      ;; are at most 9 stacks, so no risk of double-digits
                      (map #(-> (vec %)
                                (update 0 parse-long)
                                (update 1 first)
                                (update 2 first))))]
    [stacks commands]))
(comment
  (parse2 input)
  )

(defn move2 [stacks [n from to]]
  (if-let [[itms new-stack] (split-at n (get stacks from))]
    (-> stacks
        (assoc from new-stack)
        (update to #(concat itms %)))

    (throw (ex-info "Can't take from stack"
                    {:command [from to]
                     :from (get stacks from)
                     :to (get stacks to)}))))
(comment
  (move2 {\1 '(\N \Z), \2 '(\D \C \M), \3 '(\P)} [2 \2 \1])
  )

(defn part2 [input]
  (->> input
       parse2
       (apply reduce move2)
       (map (comp first second))
       str/join))
(comment
  (part2 input)
  (part2 (slurp (io/resource "day5.txt")))
  )
