(ns day7)

(def input-data
  (with-open [r (clojure.java.io/reader "day07.txt")]
    (doall (line-seq r))))

(defn bags [s]
  (re-find #"\s*([\d])*\s*(.*) bag.*" s))

(defn innermost-bag? [s]
  (re-find #".*no other bag.*" s))

(defn parse-inside-bags [inside-bags]
  (if (innermost-bag? inside-bags)
    []
    (for [b (clojure.string/split inside-bags #",")]
      (let [[_ n color] (bags b)]
        [(Integer/parseInt n) color]))))

(defn parse-row [s]
  (let [[outside-bag inside-bags] (clojure.string/split s #"contain")]
    {[ 1 (last (bags outside-bag))]
     (parse-inside-bags inside-bags)}))

(def bag-lookup
  (reduce (fn [acc r]
            (conj acc (parse-row r)))
          {}
          input-data))

(defn multiply-bags [xs multiplier]
  (reduce (fn [acc [v k]]
            (conj acc [(* v multiplier) k]))
          []
          xs))

(defn lookup [[n k :as starting-val]]
  (let [bags (get bag-lookup [1 k])]
    (if (empty? bags)
      starting-val
      {starting-val (map lookup (multiply-bags bags n))})))


(assert (= {[1 "dark blue"] [[3 "dotted white"]
                             [5 "light green"]
                             [5 "vibrant gold"]
                             [4 "mirrored lavender"]]}
           (lookup [1 "dark blue"])))
(assert (= {[10 "dark blue"] [[30 "dotted white"]
                              [50 "light green"]
                              [50 "vibrant gold"]
                              [40 "mirrored lavender"]]}
           (lookup [10 "dark blue"])))
(assert (= [1 "dotted white"]
           (lookup [1 "dotted white"])))
(assert (= [4 "dotted white"]
           (lookup [4 "dotted white"])))


(defn bag-colors [bag-tree]
  (if (map? bag-tree)
    (concat (map second (keys bag-tree))
            (first (for [v (vals bag-tree)]
                     (mapcat bag-colors v))))
    [(second bag-tree)]))


;; Answer for part 1
(dec (count (filter true?
                    (for [x (keys bag-lookup)]
                      (contains? (into #{} (bag-colors (lookup x))) "shiny gold")))))
;; => 115

;; Answer for part 2
(defn count-nested-bags [bag-tree]
  (if (map? bag-tree)
    (concat (map first (keys bag-tree))
            (first (for [v (vals bag-tree)]
                     (mapcat count-nested-bags v))))
    [(first bag-tree)]))

(dec (apply + (count-nested-bags (lookup [1 "shiny gold"]))))
;; => 1250
