(ns clj2023.interval-set
  (:import (com.google.common.collect ImmutableRangeSet)
           (com.google.common.collect Range BoundType)))

(set! *warn-on-reflection* true)

(defn range-set [& args]
  (ImmutableRangeSet/unionOf
   (map (fn [[s e]] (Range/closed s e)) args)))

(comment
  (range-set [0 5] [4 6] [0 2] [10 12])
  ;; => #object[com.google.common.collect.ImmutableRangeSet 0x7307d278 "[[0..6], [10..12]]"]
  )

(defn mark [^ImmutableRangeSet s from to]
  (.union s (range-set [from to])))

(comment
  (-> (mark (range-set) 10 15)
      (mark 0 5)
      (mark 5 9)
      (mark 20 20)
      (mark 20 22))
  ;; => #object[com.google.common.collect.ImmutableRangeSet 0x5905410d "[[0..9], [10..15], [20..22]]"]
  )
(defn erase [^ImmutableRangeSet s from to]
  (.difference s (range-set [from to])))

(comment
  (-> (range-set [0 100])
      (erase 5 10)
      (erase 20 20))
  ;; => #object[com.google.common.collect.ImmutableRangeSet 0x5b115d1d "[[0..5), (10..20), (20..100]]"]

  (-> (range-set [2 2])
      (erase 0 5))
  ;; => #object[com.google.common.collect.ImmutableRangeSet 0x6e3372b9 "[]"]
  )

(defn- close-int-range [^Range r]
  [(if (= BoundType/OPEN (.lowerBoundType r))
     (inc (.lowerEndpoint r))
     (.lowerEndpoint r))
   (if (= BoundType/OPEN (.upperBoundType r))
     (dec (.upperEndpoint r))
     (.upperEndpoint r))])

(defn to-seq [^ImmutableRangeSet s]
  (map close-int-range (.asRanges s)))

(comment
  (to-seq (-> (range-set [0 5] [10 20])
              (erase 3 5)
              (erase 15 15)))
  ;; => ([0 2] [10 14] [14 20])
  )

(defmethod print-method ImmutableRangeSet [^ImmutableRangeSet i w]
  (.write w (str "#ImmutableRangeSet \"" (.toString i) "\"")))
