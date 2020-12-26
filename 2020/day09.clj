(ns day9)


(def input-data
  (map (fn [x]
         (Long/parseLong x))
       (with-open [r (clojure.java.io/reader "day09.txt")]
         (doall (line-seq r)))))

(defn all-sums [xs]
  (into #{} (for [x xs
                  y xs
                  :when (not= x y)]
              (+ x y))))

(def PREAMBLE-SIZE 25)

(def result
  (loop [x   input-data
         acc []]
    (let [candidate (nth x PREAMBLE-SIZE)
          result    (contains? (all-sums (take PREAMBLE-SIZE x)) candidate)]

      (if (< (inc PREAMBLE-SIZE) (count x))
        (recur (drop 1 x) (conj acc [candidate result]))
        (conj acc [candidate result])))))

;; Part 1 solution
(remove second result)
;; => ([23278925 false])


(defn find-target [target xs]
  (reduce (fn [acc x]
            (let [result (+ (second (last acc)) x)]
              (cond
                (= target result) (reduced (conj acc [x result]))
                (< target result) (reduced nil)
                :else             (conj acc [x result]))))
          [[0 0]]
          xs))

(def result-section
  (drop 1 (let [target 23278925]
            (loop [xs input-data]
              (let [result (find-target target xs)]
                (if result
                  result
                  (recur (rest xs))))))))

;; Part 2 solution
(+ (apply min (map first result-section))
   (apply max (map first result-section)))
;; => 4011064
