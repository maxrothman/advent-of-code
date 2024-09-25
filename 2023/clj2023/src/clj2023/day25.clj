(ns clj2023.day25
  (:require [clj2023.util :refer [resource-lines]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def test-raw
  "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(defn parse [lines]
  (->> lines
       (map #(str/split % #"[ :]+"))
       (mapcat #(for [dst (rest %)] [(first %) dst]))
       (apply uber/graph)))

(comment

  (uber/pprint (parse (str/split-lines test-raw)))
  (def graph (resource-lines "day25.txt" parse))
  (uber/viz-graph graph {:layout :neato})
  ;; vkp - kfr, vnm - qpp, rhk - bff
  ;; separate: dxm, snq
  (def disconnected (uber/remove-edges graph ["vkp" "kfr"] ["vnm" "qpp"] ["rhk" "bff"]))
  (uber/viz-graph disconnected {:layout :neato})
  (* (count (alg/bf-traverse disconnected "dxm"))
     (count (alg/bf-traverse disconnected "snq")))
  )
