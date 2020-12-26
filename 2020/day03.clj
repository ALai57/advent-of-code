(ns day3)

(def toboggan-map
  (with-open [r (clojure.java.io/reader "day03.txt")]
    (doall (line-seq r))))

(defn coords-seq [x-step y-step [x y]]
  (lazy-seq (cons [x y]
                  (coords-seq x-step y-step
                              [(+ x x-step) (+ y y-step)]))))

(defn coords [the-map x-step y-step]
  (take-while (fn [[_ y]]
                (< y
                   (count the-map)))
              (coords-seq x-step y-step [0 0])))


(defn find-stopping-points
  [the-map x-step y-step]
  (filter (fn [[x y]]
            (= \#
               (get-in (vec the-map) [y (mod x (count (first the-map)))])))
          (coords the-map x-step y-step)))


;; Part 1 solution
(count (find-stopping-points toboggan-map 3 1))
;; => 254

;; Part 2 solution
(apply * (for [[x-step y-step] [[1 1]
                                [3 1]
                                [5 1]
                                [7 1]
                                [1 2]]]
           (count (find-stopping-points toboggan-map x-step y-step))))
;; => 1666768320
