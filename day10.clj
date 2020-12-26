(ns day10)


(def input-data
  (conj (sort (map (fn [s]
                     (Integer/parseInt s))
                   (with-open [r (clojure.java.io/reader "day10.txt")]
                     (doall (line-seq r)))))
        0))

(def voltage-diffs
  "Add 1 additional 3 V difference, because the laptop operates at 3V greater
  than the max adapter"
  (update-in (->> input-data
                  (map - (drop 1 input-data))
                  frequencies)
             [3]
             inc))

;; Part 1 solution
(apply * (vals voltage-diffs))
;; => 1876


;; Part 2 - Figure out the maximum number of 1s in a row.
(->> (map - (drop 1 input-data) input-data)
     (partition-by #{3})
     (map count)
     (apply max))


;; Since there are a maximum of four 1s before a 3, we should be able to figure
;; out how many ways we can make each sequence of 1s (e.g. how many ways can we
;; make [1 3] [1 1 3])

;; EXAMPLE 1
;; => (0 1 2 3 6)
;; =>   (1 1 1 3)
;;       2^2
;; 4

;; EXAMPLE 2
;; => (0 1 2 3 4 7)
;; =>   (1 1 1 1 3)
;;       1 2^2
;;       X 2^2-1
;; 7


;; EXAMPLE 3
;; => (0 1 2 3 4  5 8)
;; =>   (1 1 1 1  1 3)
;;       2 1 2^2
;; 8
;;
;;       1 X 2^2-1
;; 7
;;       X X 1 2
;;
;;
;; = 17


;; Sequence of voltage diffs -> How many ways can we arrange our adapters to
;; make this work?
{"3" 1
 "13" 1
 "113" 2
 "1113" 4
 "11113" 7}

(def ->combinations
  {"" 1
   "1" 1
   "11" 2
   "111" 4
   "1111" 7})

;; PART 2
(reduce *
        (map ->combinations
             (clojure.string/split (apply str (map - (drop 1 input-data) input-data))
                                   #"3")))
;; => 14173478093824



;; Testing
(map count (partition-by #{3} (map - (drop 1 input-data) input-data)))


(def test-data-0
  (conj (sort [16 10 15 5 1
               11 7 19 6 12
               4])
        0))

(def test-data
  (conj (sort [28 33 18 42 31
               14 46 20 48 47
               24 23 49 45 19
               38 39 11 1  32
               25 35 8  17 7
               9  4  2  34 10
               3])
        0))


(count test-data)
;; => 32

(frequencies (map - (drop 1 test-data) test-data))
;; => {1 22, 3 9}
