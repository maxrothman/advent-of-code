(ns advent-of-code.util)

(defmacro fork
  "(f (g x) (h x))"
  ;; Only a macro because some common fns like 'or' are macros
  [f g h x]
  `(~f (~g ~x) (~h ~x)))

;; In core in a newer version of Clojure
(defn map-vals [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

;; In core in a newer version of Clojure
(defn map-keys [f m]
  (reduce-kv #(assoc %1 (f %2) %3) {} m))

(defn apply-when [pred f x]
  (if (pred x)
    (f x)
    x))

(defn until [pred f x]
  (->> (iterate f x)
       (drop-while (complement pred))
       first))

(defn flip [f a b & rest]
  (apply f b a rest))

(defn group-with [key-f val-f coll]
  (->> coll
       (group-by key-f)
       (map-vals (partial map val-f))))