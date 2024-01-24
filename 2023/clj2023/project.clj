(defproject clj2023 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[com.clojure-goes-fast/clj-async-profiler "1.1.1"]
                 [com.cnuernber/charred "1.033"]
                 [com.github.flow-storm/flow-storm-dbg "3.9.1"]
                 [dev.weavejester/medley "1.7.0"]
                 [dom-top "1.0.9"]
                 [io.helins/interval "1.0.0-beta0"]
                 [mvxcvi/arrangement "2.1.0"]
                 [net.mikera/core.matrix "0.63.0"]
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/core.logic "1.0.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/math.combinatorics "0.2.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.flatland/ordered "1.15.11"]]
  ;; For clj-async-profiler
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])
