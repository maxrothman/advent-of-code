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

(defn spyf [f]
  (fn spied [& args]
    (prn args)
    (apply f args)))

(defn >->>
  "Call f as if in thread-last within thread-first"
  [x f & args]
  (apply f (conj args x)))

(defn >>->
  "Call f as if in thread-first within thread-last"
  [f & args]
  (let [x (last args)]
    (apply f x (butlast args))))

(defn until
  "Iterate f on x until (pred x) is true"
  [pred f x]
  (->> (iterate f x)
       (drop-while (complement pred))
       first))

(defn progress [s]
  (let [r (doall
           (map-indexed (fn [i x]
                          (if (zero? (mod i 10))
                            (do (print ".") (flush) x)
                            x))
                        s))]
    (println)
    r)
  )

(defn fork [f g h x]
  (f (g x) (h x)))