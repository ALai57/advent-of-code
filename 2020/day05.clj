(ns day5)

(defn ->row [s]
  (Integer/parseInt (apply str (map {\B 1 \F 0} s)) 2))


(assert (= 70 (->row "BFFFBBF")))
(assert (= 14 (->row "FFFBBBF")))
(assert (= 102 (->row "BBFFBBF")))

(defn ->col [s]
  (Integer/parseInt (apply str (map {\L 0 \R 1} s)) 2))

(assert (= 7 (->col "RRR")))
(assert (= 4 (->col "RLL")))

(def input-data
  (with-open [r (clojure.java.io/reader "day05.txt")]
    (doall (line-seq r))))

(defn ->seat-id [s]
  (let [r (->row (take 7 s))
        c (->col (drop 7 s))]
    (+ (* 8 r) c)))

(assert (= 567 (->seat-id "BFFFBBFRRR")))
(assert (= 119 (->seat-id "FFFBBBFRRR")))
(assert (= 820 (->seat-id "BBFFBBFRLL")))

;; Part 1 solution
(apply max (map ->seat-id input-data))
;; => 998


(def sorted-seats
  (sort (map ->seat-id input-data)))

;; Part 2 solution
(.indexOf (map (fn [x y]
                 (- y x)) sorted-seats
               (drop 1 sorted-seats))
          2)
;; => 637
