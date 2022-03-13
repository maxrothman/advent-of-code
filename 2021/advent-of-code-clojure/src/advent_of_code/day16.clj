(ns advent-of-code.day15
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [advent-of-code.util :as util]))

;; Left off: first test passed!

;;;;;;;;;;
;; Helpers
;;;;;;;;;;

(defn hex->bin [h]
  (-> h
      (BigInteger. 16)
      (.toString 2)))

(defn bin->num [x]
  (BigInteger. (apply str x) 2))

;;;;;;;;;;;;;;;;
;; State machine
;;;;;;;;;;;;;;;;

(defn finalize-lit [{:keys [lit-accum :result] :as r}]
  (-> r
      (dissoc :lit-accum)
      (assoc :next :end)
      (update :result assoc :val (-> lit-accum
                                     reverse
                                     bin->num))))

(defn lit [{:keys [stream lit-accum result]}]
  (let [[[continue? & group] new-stream] (split-at 5 stream)]
    {:stream new-stream
     :next (if (= \1 continue?) lit finalize-lit)
     :lit-accum (into lit-accum group)
     :result result}))

(defn op [{:keys []}])

(defn packet-type [{:keys [stream result]}]
  (let [[type-bytes new-stream] (split-at 3 stream)
        typ (bin->num type-bytes)]
    (if (= 4 typ)
      {:stream new-stream
       :next lit
       :result (assoc result :type :lit)}
      {:stream new-stream
       :next op
       :result (assoc result :type :op)})))

(defn packet-version [{:keys [stream] :as r}]
  (let [[version-bytes new-stream] (split-at 3 stream)
        version (bin->num version-bytes)]
    (merge r
           {:stream new-stream
            :result {:version version}
            :next packet-type})))

(defn interpret [bits]
  #_(iterate #((:next %) %) (packet-version {:stream bits}))
  (-> (util/until #(-> % :next (= :end))
                  #((:next %) (dissoc % :next))
                  (packet-version {:stream bits}))
      :result))

(comment
  (->> "D2FE28"
       hex->bin
       interpret
       (take 6)
       (map #(update % :stream (partial apply str)))))

;;;;;;;;;;;;;;;;
;; Debug helpers
;;;;;;;;;;;;;;;;

(defn spy [x] (prn x) x)

(defn =ish [x y]
  (cond
    (and (map? x) (map? y)) (->> (seq x)
                                 (map (fn [[k v]] [k v (get y k)]))
                                 (map (fn [[k v1 v2]] (=ish v1 v2)))
                                 (every? identity))

    (and (sequential? x) (sequential? y)) (->> (map =ish x y)
                                               (every? identity))
    :else (= x y)))

(comment
  (=ish {:x 1 :y [1]} {:x 1 :y [1]})
  ;; => true

  (=ish {:type :op
         :subs [{:type :lit}
                {:type :op
                 :subs [{:type :lit}
                        {:type :lit
                         :val 1}]}]}
        {:type :op
         :version 1
         :subs [{:type :lit
                 :val 4}
                {:type :op
                 :val 6
                 :subs [{:type :lit
                         :val 2}
                        {:type :lit
                         :version 5
                         :val 1}]}]})
  ;; => true
  )

;;;;;;;;;;;;;;;;;;
;; Part 1 solution
;;;;;;;;;;;;;;;;;;

(defn version-sum [ast])

;;;;;;;;
;; Tests
;;;;;;;;

(comment
  (do
    (is (=ish {:type :lit, :val 2021, :version 6}
              (interpret "110100101111111000101000")))
    (is (=ish {:type :op
               :val 6
               :version 1
               :subs [{:type :lit, :val 10}
                      {:type :lit, :val 20}]}
              (interpret "00111000000000000110111101000101001010010001001000000000")))
    (is (=ish {:type :op
               :val 3
               :version 7
               :subs [{:type :lit, :val 1}
                      {:type :lit, :val 2}
                      {:type :lit, :val 3}]}
              (interpret "11101110000000001101010000001100100000100011000001100000")))
    (let [ast (interpret (hex->bin "8A004A801A8002F478"))]
      (is (=ish {:type :op
                 :version 4
                 :subs [{:type :op
                         :version 1
                         :subs [{:type :op
                                 :version 5
                                 :subs [{:type :lit
                                         :version 6}]}]}]}
                ast))
      (is (= 16 (version-sum ast))))
    (let [ast1 (interpret (hex->bin "620080001611562C8802118E34"))
          ast2 (interpret (hex->bin "C0015000016115A2E0802F182340"))]
      (is (=ish {:type :op
                 :version 3
                 :subs [{:type :op
                         :subs [{:type :lit}
                                {:type :lit}]}
                        {:type :op
                         :subs [{:type :lit}
                                {:type :lit}]}]}
                ast1))
      (is (= 12 (version-sum ast1)))

      (is (=ish {:type :op
                 :version 3
                 :subs [{:type :op
                         :subs [{:type :lit}
                                {:type :lit}]}
                        {:type :op
                         :subs [{:type :lit}
                                {:type :lit}]}]}
                ast2))
      (is (= 23 (version-sum ast2)))
      (is (not= (:length-type ast1) (:length-type ast2))))

    (let [ast (interpret (hex->bin "A0016C880162017C3686B18A3D4780"))]
      (is (=ish {:type :op
                 :subs [{:type :op
                         :subs [{:type :op
                                 :subs [{:type :lit}
                                        {:type :lit}
                                        {:type :lit}
                                        {:type :lit}
                                        {:type :lit}]}]}]}
                ast))
      (is (= 31 (version-sum ast))))))