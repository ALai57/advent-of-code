(ns day6)

(def input-data
  (map (fn [s] (clojure.string/split s #"\s"))
       (clojure.string/split (slurp "day06.txt") #"\n\n")))

(comment
  (into #{} (seq (apply str (first input-data))))
  ;; => #{\r \t}
  )

(apply + (map count (for [x input-data]
                      (into #{} (apply str x)))))
;; => 6310

(apply + (map count (map #(apply clojure.set/intersection %)
                         (for [x input-data]
                           (map set x)))))
;; => 3193
