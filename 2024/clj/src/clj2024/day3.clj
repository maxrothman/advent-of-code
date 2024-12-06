(ns clj2024.day3
  (:require
   [clj2024.util :refer [resource-lines]]
   [clojure.string :as str]))

(def test-raw
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn tokenize [raw]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" raw))

(comment
  (tokenize test-raw))

(defn execute [tokenized]
  (->> tokenized
       (map (fn [[_ a b]] (* (parse-long a) (parse-long b))))
       (apply +)))

(comment
  (execute (tokenize test-raw))
  (execute (tokenize (slurp "resources/day3.txt")))
  )

(def test-raw2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn tokenize2 [raw]
  (let [m (re-matcher
           #"(?:(?<mul>mul)\((?<a>\d{1,3}),(?<b>\d{1,3})\)|(?<do>do)\(\)|(?<dont>don't)\(\))"
           raw)]
    (->> (repeatedly #(if (.find m) m nil))
         (take-while some?)
         (map #(let [base {:match (.group %)
                           :fname (or (.group % "mul")
                                      (.group % "do")
                                      (.group % "dont"))}]
                 (if (= "mul" (:fname base))
                   (merge base
                          {:a (.group % "a")
                           :b (.group % "b")})
                   base))))))

(comment
  (tokenize2 test-raw2)
  )

(defn execute2 [instrs]
  (reduce (fn [acc x]
            (case (:fname x)
              "mul" (cond-> acc
                      (:on acc)
                      (update :sum +
                              (* (parse-long (:a x))
                                 (parse-long (:b x)))))
              "do" (assoc acc :on true)
              "don't" (assoc acc :on false)))
          {:on true, :sum 0} instrs))

(comment
  (execute2 (tokenize2 test-raw2))
  (execute2 (tokenize2 (slurp "resources/day3.txt")))
  )