(ns clj2024.util 
  (:require
   [clojure.java.io :as io]))

(defn spy
  ([x] (prn x) x)
  ([msg x] (prn msg x) x))

(defn resource-lines [fname func]
  (with-open [f (io/reader (io/resource fname))]
    (func (line-seq f))))

(defn fork [f g h x]
  (f (g x) (h x)))

(defn map2 
  ([f coll] (map2 f identity coll))
  ([f2 f1 coll] (map (comp f1 #(map f2 %)) coll)))
