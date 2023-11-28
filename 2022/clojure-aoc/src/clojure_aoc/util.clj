(ns clojure-aoc.util 
  (:require [clojure.java.io :as io]))

(defn read-input [dir]
  (slurp (io/resource (str dir "/input.txt"))))

(defn map2 [f coll]
  (map #(map f %) coll))

(defn descending [x y] (compare y x))

(defn spy
  ([x]
   (prn x)
   x)
  ([msg x]
   (prn msg x)
   x))

(defmacro as->> [& form]
  `(as-> ~(last form) ~@(butlast form)))

(defn fork [f g h]
  #(f (g %) (h %)))

(defn over
  "(f (g x) (g y) ..."
  [f g]
  #(apply f (map g %)))

(defmacro defs [& args]
  (when-not (even? (count args))
    (throw (ex-info "Must have an even number of forms" {})))
  (let [pairs (partition 2 args)]
    `(do ~@(for [[name form] pairs] `(def ~name ~form)))))
