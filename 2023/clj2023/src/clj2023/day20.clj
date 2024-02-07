(ns clj2023.day20
  (:require [clj2023.util :refer [until]]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [lcm]]
            [clojure.string :as str]
            [medley.core :refer [map-kv-vals queue]]))

(defn parse-line [line]
  (let [[module dests] (map str/trim (str/split line #"->"))
        mod-type (case (first module)
                   \% :flip
                   \& :conj
                   \b :broadcast)
        mod-name (if (= "broadcaster" module)
                   "broadcaster"
                   (subs module 1))]
    [mod-name {:name mod-name
               :typ mod-type
               :dst (str/split dests #", ")}]))

(defn parse [raw]
  (let [parsed (into {} (map parse-line) (str/split-lines raw))
        inverse (reduce-kv (fn [m k {:keys [dst]}]
                             (reduce #(update %1 %2 conj k) m dst))
                           {} parsed)]
    (map-kv-vals (fn [k v]
                   (cond-> v
                     (= :conj (:typ v))
                     (assoc :state (zipmap (inverse k) (repeat :lo)))))
                 parsed)))

(comment
  @(def test-data1
     (parse "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"))

  @(def test-data2
     (parse "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")))

(defmulti do-pulse (fn [module _pulse] (:typ module)))

(defmethod do-pulse :broadcast [{:keys [dst name]} {:keys [typ]}]
  {:pulses (map #(hash-map :src name, :dst %, :typ typ) dst)})

(defmethod do-pulse :flip [{:keys [dst on name]} {:keys [typ]}]
  (when (= :lo typ)
    {:pulses (map #(hash-map :src name, :dst %, :typ (if on :lo :hi)) dst)
     :module {:on (not on)}}))

(defmethod do-pulse :conj [{:keys [dst state name]} {:keys [src typ]}]
  (let [state' (assoc state src typ)]
    {:pulses (map #(hash-map :src name, :dst %
                             :typ (if (every? (partial = :hi) (vals state')) :lo :hi)) 
                  dst)
     :module {:state state'}}))

(defn step [tracer
            {:keys [modules], [{:keys [dst] :as p} :as pulses] :pulses :as state}]
  (let [{module' :module, pulses' :pulses} (do-pulse (modules dst) p)]
    (-> state
        (update-in [:modules dst] merge module')
        ;; Remove pulses to modules outside the known set
        (assoc :pulses (into (pop pulses) (filter (comp modules :dst) pulses')))
        (tracer pulses'))))

(defn press-button [tracer state] 
  (-> (until (comp empty? :pulses)
             (partial step tracer)
             (-> state
                 (assoc :pulses (queue [{:dst "broadcaster" :typ :lo}]))
                 (tracer [{:dst "broadcaster" :typ :lo}])))
      (dissoc :pulses)))

(defn pt1 [data]
  (-> (iterate (partial press-button
                        (fn tracer [state pulses]
                          (-> state
                              (update-in [:pulse-ct :lo] + (count (filter #(= :lo (:typ %)) pulses)))
                              (update-in [:pulse-ct :hi] + (count (filter #(= :hi (:typ %)) pulses))))))
               {:modules data
                :pulse-ct {:lo 0, :hi 0}})
      (nth 1000)
      vals
      (as-> $ (apply * $))))

(comment
  (pt1 test-data1)
  (pt1 test-data2)
  (pt1 (parse (slurp (io/resource "day20.txt"))))
  )

(defn pt2 [data]
  (->> (iterate (comp (partial press-button
                               (fn [state pulses]
                                 (->> (filter #(and (get (get-in state [:trace :watching]) (:src %))
                                                    (= :hi (:typ %)))
                                              pulses)
                                      (map #(vector (:src %) (get-in state [:trace :presses])))
                                      (into {})
                                      ;; Merge in the existing state second so the shortest cycle is
                                      ;; always kept
                                      (update-in state [:trace :cycle] #(merge %2 %1)))))
                      #(update-in % [:trace :presses] inc))
                {:modules data
                 :trace {:watching #{"jf" "sh" "mz" "bh"}
                         :presses 0
                         :cycle {}}})
       (drop-while #(> 4 (count (get-in % [:trace :cycle]))))
       first
       :trace))

(comment
  (def cycles (pt2 (parse (slurp (io/resource "day20.txt")))))
  (->> cycles
       :cycle
       vals
       (reduce lcm))
  
  ;; Mermaid
  (->> (for [[src module] (parse (slurp (io/resource "day20.txt")))
             dst (:dst module)]
         (str src " -> " dst ";"))
       (str/join "\n")
       println)
  (->> (parse (slurp (io/resource "day20.txt")))
       vals
       (filter #(= :conj (:typ %)))
       (map :name)
       (str/join ", "))
  (def state
    (-> (parse (slurp (io/resource "day20.txt")))
        (merge {:trace {:watching #{"jf" "sh" "mz" "bh"}
                        :presses 0
                        :cycle {}}})))
  )