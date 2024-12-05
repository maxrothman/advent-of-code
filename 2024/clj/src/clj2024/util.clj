(ns clj2024.util 
  (:require
   [clojure.java.io :as io]))

(defn spy [x] (prn x))

(defn resource-lines [fname func]
  (with-open [f (io/reader (io/resource fname))]
    (func (line-seq f))))