(ns clojure-aoc.day7
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(defn spy
  ([x]
   (prn x)
   x)
  ([x msg]
   (prn msg x)
   x))

(def input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn fs-zip [m]
  (zip/zipper #(= :d (:type %))
              (comp seq :children)
              #(assoc %1 :children (vec %2))
              m))

(defn dir [name]
  {:name name :type :d :children []})

(defn file [size name]
  {:name name :type :f :size size})

(comment
  (-> (fs-zip {:type :d
               :name "/"
               :children [{:type :d :name "foo"
                           :children [{:type :f :name "aaa"}]}
                          {:type :f :name "bar"}
                          {:type :f :name "baz"}]})
      zip/down
      (zip/insert-right {:type :f :name "bbb"})
      zip/right
      zip/root
      fs-zip)
  )

(defn down-to
  "Return the loc of the child of this loc matching pred, or nil if none match"
  [loc pred]
  (->> loc
       zip/down
       (iterate zip/right)
       (drop-while #(some-> % zip/node pred not))
       first))
(comment
  (-> (fs-zip {:name "/" :type :d :children [{:name "a"} {:name "d"} {:name "c"}]})
      (down-to #(= "b" (:name %))))
  ;; => nil

  (-> (fs-zip {:name "/" :type :d :children [{:name "a"} {:name "b"} {:name "c"}]})
      (down-to #(= "b" (:name %)))
      zip/node)
  ;; => {:name "b"}

  )

(defn node-matches [a b]
  (= (:name a) (:name b)))

(defn down-or-create [fs node]
  (down-to
   (if (some #(node-matches node %) (zip/children fs))
     fs
     (zip/append-child fs node))
   #(node-matches node %)))

(defn parse-ls-line [fs line]
  (zip/up
   (if-let [match (re-matches #"dir (.+)" line)]
     (down-or-create fs (dir (second match)))
     (down-or-create fs (-> (str/split line #" " 2)
                            (update 0 parse-long)
                            (as-> $ (apply file $)))))))
(comment
  (parse-ls-line (fs-zip (dir "/")) "dir hi")
  )

(defn parse 
  ([input]
   (parse (fs-zip (dir "/")) input))
  ([fs [line & rst]]
   (let [cd-match (delay (re-matches #"\$ cd (.*)" line))]
     (cond
       (nil? line) (fs-zip (zip/root fs))
       (= "$ cd /" line) (-> (zip/root fs) fs-zip (recur rst))
       (= "$ cd .." line) (recur (zip/up fs) rst)
       (some? @cd-match) (-> (down-or-create fs (dir (second @cd-match)))
                             (recur rst))
       (= "$ ls" line) (-> (split-with #(not (str/starts-with? % "$")) rst)
                           (update 0 #(reduce parse-ls-line fs %))
                           (as-> $ (let [[fs rst] $] (recur fs rst))))))))
(comment
  (parse "$ cd /
$ cd foo
$ cd bar
$ ls
dir a
3312 hi
dir b
98787 hihi
$ cd /")
  (split-with #(not (str/starts-with? % "$")) ["a" "bc" "$ a"])

  (-> (fs-zip {:name "/" :type :d :children []})
      zip/root
      zip/branch?)

  (split-with keyword? [:a :b 1 :b])
  
  (parse (str/split-lines input))
  )

;; - / (dir)
;;   - a (dir)
;;     - e (dir)
;;       - i (file, size=584)
;;     - f (file, size=29116)
;;     - g (file, size=2557)
;;     - h.lst (file, size=62596)
;;   - b.txt (file, size=14848514)
;;   - c.dat (file, size=8504156)
;;   - d (dir)
;;     - j (file, size=4060174)
;;     - d.log (file, size=8033020)
;;     - d.ext (file, size=5626152)
;;     - k (file, size=7214296)

;; From https://stackoverflow.com/a/58700831/1901786
(defn post-zip
  [loc]
  ;; start from the deepest left child
  (loop [loc loc]
    (if-let [l (and (zip/branch? loc) (zip/down loc))]
      (recur l)
      loc)))

(defn post-next
  [loc]
  (if-let [sib (zip/right loc)]
    ;; if we have a right sibling, move to it's deepest left child
    (post-zip sib)
    ;; If there is no right sibling move up if possible
    (if-let [parent (zip/up loc)]
      parent
      ;; otherwise we are done
      (with-meta [(zip/node loc) :end] (meta loc)))))

(defn postwalk [f zipper]
  (loop [loc (post-zip zipper)]
    (if (zip/end? loc)
      (assoc loc 1 nil)
      (recur (post-next (zip/replace loc (f (zip/node loc))))))))

(comment
  (->> (parse (str/split-lines input))
       (zip-walk #(cond-> % (:size (zip/node %)) (zip/edit update :size inc))))
  (->> (parse (str/split-lines input))
       (zip-walk #(do (prn (:name (zip/node %)) (% 1)) %)))
  (-> (parse (str/split-lines input))
      zip/next
      zip/next
      zip/next
      zip/node)
  (-> (parse (str/split-lines input))
      #_(zip/vector-zip [[]])
      zip/down
      zip/down
      zip/node)
  (postwalk spy (parse (str/split-lines input)))
  (postwalk identity (zip/vector-zip [[]])))

(defn part1 [fs]
  (->> fs
       (postwalk #(if (:children %)
                    (assoc % :size (->> % :children (map :size) (apply +)))
                    %))
       (iterate zip/next)
       (take-while (complement zip/end?))
       (map zip/node)
       (filter #(= :d (:type %)))
       (map :size)
       (filter #(<= % 100000))
       (apply +)))
(comment
  (->> (parse (str/split-lines input))
       part1)
  ;; => 95437
  
  (with-open [f (io/reader (io/resource "day7.txt"))]
    (-> (parse (line-seq f))
        part1))
  )

;; Part 2

(defn part2 [fs]
  (let [fs (postwalk #(if (:children %)
                        (assoc % :size (->> % :children (map :size) (apply +)))
                        %)
                     fs)
        space-needed (- 30000000 (- 70000000 (:size (zip/node fs))))]
    (->> fs
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (filter #(= :d (:type %)))
         (filter #(< space-needed (:size %)))
         (apply min-key :size)
         ((juxt :name :size)))))
(comment
  (with-open [f (io/reader (io/resource "day7.txt"))]
    (-> (parse (line-seq f))
        part2))
  ;; => ["mbsjjzft" 1498966]

  )
