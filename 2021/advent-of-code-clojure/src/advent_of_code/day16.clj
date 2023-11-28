(ns advent-of-code.day15
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as st]
            [advent-of-code.util :as util]))

;;;;;;;;;;
;; Helpers
;;;;;;;;;;

(defn hex->bin [h]
  (-> h
      (BigInteger. 16)
      (.toString 2)))

(defn bin->num [x]
  (BigInteger. (apply str x) 2))

;;;;;;;;
;; Specs
;;;;;;;;
(defn not-keys [& ks]
  #(not-any? (partial contains? %) ks))

(s/def :ctx/stream (s/coll-of char?))
(s/def :ctx/next (s/or :fn #(fn? (deref %))
                       :end #{:end}))
(s/def :ctx/stack (s/and list? (s/coll-of map?)))
(s/def :ctx/ctx (s/keys :req-un [:ctx/stream :ctx/stack]
                        :opt-un [:ctx/next]))

(s/def :with-verison/version integer?)
(s/def :with-version/result (s/keys :req-un [:packet/version]))
(s/def :with-version/ctx (s/and :ctx/ctx
                                (s/keys :req-un [:with-version/result])))

(s/def :with-type/type #{:lit :op})
(s/def :with-type/result (s/keys :req-un [:with-version/type]))
(s/def :with-type/ctx (s/and :with-version/ctx
                             (s/keys :req-un [:with-type/result])))

(s/def :lit/lit-accum (s/coll-of char?))
(s/def :lit/type #{:lit})
(s/def :lit/result (s/keys :req-un [:lit/type]))
(s/def :lit/ctx (s/and :with-version/ctx
                       (s/keys :req-un [:lit/result]
                               :opt-un [:lit/lit-accum])))

(s/def :lit-complete/val integer?)
(s/def :lit-complete/result (s/keys :req-un [:lit-complete/val]))
(s/def :lit-complete/ctx (s/and :with-version/ctx
                                (s/keys :req-un [:lit-complete/result])
                                (not-keys :lit-accum)))

(s/def :op/stack seq)  ;Not empty
(s/def :op/ctx (s/and (s/keys :req-un [:op/stack])
                      :ctx/ctx))

(s/def :op-bit-length/length-type #{:bits})
(s/def :op-bit-length/specified-length integer?)
(s/def :op-bit-length/stream-size integer?)
(s/def :op-bit-length/ctx (s/and (s/keys :req-un [:op-bit-length/length-type
                                                  :op-bit-length/specified-length
                                                  :op-bit-length/stream-size])
                                 :op/ctx))

(s/def :op-packet-length/length-type #{:packets})
(s/def :op-packet-length/specified-length integer?)
(s/def :op-packet-length/ctx (s/and :op/ctx
                                    (s/keys :req-un [:op-packet-length/length-type
                                                     :op-packet-length/specified-length])))

(s/def :in-subpacket/ctx (s/or :bit-length :op-bit-length/ctx
                               :packet-length :op-packet-length/ctx))
;;TODO working on end-of-packet
(comment
  start-of-packet
  (end-of-lit {:stream '(\0 \0 \0),
               :stack (),
               :result {:version 4, :type :lit},
               :lit-accum '(\1 \0 \1 \0 \0 \1 \0 \1 \0 \1 \0 \1 \0 \1 \0 \1)}))
;;;;;;;;;;;;;;;;
;; State machine
;;;;;;;;;;;;;;;;

(declare start-of-packet packet-type lit end-of-lit start-of-op op-length-bits op-length-packets end-of-op end-of-packet)

(defn-spec start-of-packet :with-version/ctx
  [{:keys [stream] :as ctx} :ctx/ctx]
  (let [[version-bytes new-stream] (split-at 3 stream)
        version (bin->num version-bytes)]
    (merge ctx
           {:stream new-stream
            :result {:version version}
            :next #'packet-type})))

(defn-spec packet-type :with-type/ctx
  [{:keys [stream result] :as ctx} :ctx/ctx]
  (let [[type-bytes new-stream] (split-at 3 stream)
        typ (bin->num type-bytes)]
    (merge ctx
           (if (= 4 typ)
             {:stream new-stream
              :next #'lit
              :result (assoc result :type :lit)}
             {:stream new-stream
              :next #'start-of-op
              :result (assoc result :type :op)}))))

(defn-spec lit :lit/ctx
  [{:keys [stream lit-accum result] :as ctx} :with-type/ctx]
  (let [[[continue? & group] new-stream] (split-at 5 stream)]
    (merge ctx
           {:stream new-stream
            :next (if (= \1 continue?) #'lit #'end-of-lit)
            :lit-accum (into lit-accum group)
            :result result})))

(defn-spec end-of-lit :lit-complete/ctx
  [{:keys [lit-accum] :as ctx} :lit/ctx]
  (-> ctx
      (dissoc :lit-accum)
      (assoc :next #'end-of-packet)
      (update :result assoc :val (-> lit-accum
                                     reverse
                                     bin->num))))

(defn-spec start-of-op (s/and :op/ctx (not-keys :result))
  {:fn #(=ish (-> % :args :arg-0 :result) (-> % :ret :stack first))}
  [{[length-type & stream] :stream
    :keys [result]
    :as ctx}
   :with-version/ctx]
  (-> ctx
      ;; TODO BUG
      ;; This line doesn't appear to be working. :subs is never assoc'd, stack
      ;; is empty
      ;;
      ;; Wait, did I just fix it by removing some dead code?
      (update :stack conj (assoc result :subs []))  ;Push op onto stack
      (dissoc :result)                              ;
      (assoc :stream stream)
      (assoc :next (if (= \0 length-type)
                     #'op-length-bits
                     #'op-length-packets))))

(defn-spec op-length-bits (s/and :op-bit-length/ctx (not-keys :result))
  [{:keys [stream stack result] :as ctx} :op/ctx]
  (let [[length-bits new-stream] (split-at 15 stream)
        length (bin->num length-bits)]
    (merge ctx
           {:stream new-stream
            :length-type :bits
            :specified-length length
            :stream-size (count stream)
            :next #'start-of-packet})))

(defn-spec op-length-packets (s/and :op-packet-length/ctx (not-keys :result))
  [{:keys [stream stack result] :as ctx} :op/ctx]
  (let [[length-bits new-stream] (split-at 11 stream)
        length (bin->num length-bits)]
    (merge ctx
           {:stream new-stream
            :length-type :packets
            :specified-length length
            :next #'start-of-packet})))

(defn end-of-packet
  [{[s & ss] :stack
    :keys [length-type specified-length stream-size stream result]
    :as ctx}]
  (let [specd-length-hit (if (= :bits length-type)
                           (= specified-length
                              (- stream-size (count stream)))
                           (= specified-length (count (:subs s))))
        end? (every? #(= \0 %) stream)]
    (cond
      ;; Not in a subpacket
      (nil? s) (merge ctx {:next :end})
      ;; TODO: what if end of op AND end of stream?
      ;; Maybe need a separate state for checking stream end

      ;; End of op
      specd-length-hit
      (-> ctx
          (assoc :result s)  ;Pop off the stack into :result
          (assoc :stack ss)  ;
          (update-in [:result :subs] conj result)  ;Record last packet
          (dissoc :length-type :specified-length :stream-size)
          (assoc :next (if end? :end #'start-of-packet)))

      ;; Op continues
      :else
      (-> ctx
          (assoc :stack (->> result
                             (update s :subs conj)
                             (conj ss)))
          (dissoc :result)
          (assoc :next (if end? :end #'start-of-packet))))))

(defn interpret [bits]
  (-> (util/until #(-> % :next (= :end))
                  #((:next %) (dissoc % :next))
                  {:stream bits, :stack '(), :next start-of-packet})
      :result))

(defn interpret-debug [bits]
  (iterate #((:next %) %) {:stream bits, :stack '(), :next start-of-packet}))

(comment
  (->> (interpret-debug "00111000000000000110111101000101001010010001001000000000")
       (take 16)
       (map #(update % :stream (partial apply str))))

  (->> "D2FE28"
       hex->bin
       interpret-debug
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
              (interpret (seq "110100101111111000101000"))))
    (is (=ish {:type :op
               :val 6
               :version 1
               :subs [{:type :lit, :val 10}
                      {:type :lit, :val 20}]}
              (interpret (seq "00111000000000000110111101000101001010010001001000000000"))))
    (is (=ish {:type :op
               :val 3
               :version 7
               :subs [{:type :lit, :val 1}
                      {:type :lit, :val 2}
                      {:type :lit, :val 3}]}
              (interpret (seq "11101110000000001101010000001100100000100011000001100000"))))
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

(st/instrument)