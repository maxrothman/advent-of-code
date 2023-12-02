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