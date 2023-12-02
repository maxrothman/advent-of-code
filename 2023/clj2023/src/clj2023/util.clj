(ns clj2023.util)

(defn map2 [f coll]
  (map #(map f %) coll))

(defmacro as->> [& form]
  `(as-> ~(last form) ~@(butlast form)))