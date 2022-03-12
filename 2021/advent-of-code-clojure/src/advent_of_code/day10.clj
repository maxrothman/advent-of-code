(ns advent-of-code.day10)

(def cases [["{([(<{}[<>[]}>{[]{[(<()>" \}]
            ["[[<[([]))<([[{}[[()]]]" \)]
            ["[{[{({}]{}}([{[{{{}}([]" \]]
            ["[<(<(<(<{}))><([]([]()" \)]
            ["<{([([[(<>()){}]>(<<{{" \>]])

(def pairs {\( \)
            \[ \]
            \{ \}
            \< \>})

(defn balanced
  ([chars] (balanced '() chars))
  ([[s & ss :as stack] [c & chars]]
   (cond
     (and (nil? c) (nil? s)) {:type :balanced}
     (contains? pairs c) (recur (conj stack c) chars)
     (= c (pairs s)) (recur ss chars)
     (and (nil? c) (some? s)) {:type :incomplete, :remaining stack}
     :else {:type :unbalanced, :char c})))

(comment
  (map #(-> (first %)
            balanced
            :char
            (= (second %)))
       cases)
  )

(def score-unbalanced
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def test-data
  ["[({(<(())[]>[[{[]{<()<>>"
   "[(()[<>])]({[<{<<[]>>("
   "{([(<{}[<>[]}>{[]{[(<()>"
   "(((({<>}<{<{<>}{[]{[]{}"
   "[[<[([]))<([[{}[[()]]]"
   "[{[{({}]{}}([{[{{{}}([]"
   "{<[[]]>}<{[{[{[]{()[[[]"
   "[<(<(<(<{}))><([]([]()"
   "<{([([[(<>()){}]>(<<{{"
   "<{([{{}}[<[[[<>{}]]]>[]]"])

(def data
  (with-open [f (clojure.java.io/reader "resources/day10/input.txt")]
    (vec (line-seq f))))

;; Part 1
(comment
  (->> data
       (map balanced)
       (filter #(= :unbalanced (:type %)))
       (map :char)
       (map score-unbalanced)
       (reduce +)))

;; Part 2
(defn score-incomplete [chars]
  (let [score-vals {\) 1
                    \] 2
                    \} 3
                    \> 4}]
    (reduce #(+ (score-vals %2) (* 5 %1))
            0
            chars)))

(comment
  (score-incomplete "}}]])})]")
  ;; => 288957
)

(comment
  (->> data
       (map balanced)
       (filter (comp #{:incomplete} :type))
       (map :remaining)
       (map #(map pairs %))
       (map score-incomplete)
       sort
       (#(nth % (/ (count %) 2)))))