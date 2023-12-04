(ns clj2023.util
  (:require [clojure.java.io :as io]))

(defn map2 [f coll]
  (map #(map f %) coll))

(defmacro as->> [& form]
  `(as-> ~(last form) ~@(butlast form)))

(defn resource-lines [fname func]
  (with-open [f (io/reader (io/resource fname))]
    (func (line-seq f))))

(defn spy [x]
  (prn x)
  x)

(defn >->>
  "Adapt f to thread-first"
  [x f & args]
  (apply f (concat args (list x))))

(defn >>->
  "Adapt f to thread-last"
  [f & args]
  (let [x (last args)]
    (apply f x (butlast args))))