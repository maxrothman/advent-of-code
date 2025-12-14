(ns util 
  (:require
   [clojure.java.io :as io]))

(defn resource-lines [fname func]
  (with-open [f (io/reader (io/resource fname))]
    (func (line-seq f))))
