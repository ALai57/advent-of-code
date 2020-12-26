(ns day11)

(def input-data
  (into []
        (with-open [r (clojure.java.io/reader "day11.txt")]
          (doall (line-seq r)))))

(defn get-row [point]
  (first point))

(defn get-col [point]
  (second point))

(defn neighbors [seat-map [row col]]
  (->> (for [drow [-1 0 1]
             dcol [-1 0 1]
             :when (not= 0 drow dcol)]
         (map + [row col] [drow dcol]))
       (map (fn [point]
              (get-in seat-map point nil)))))

(defn advance [seat-map point]
  (let [neighbor-seats (frequencies (neighbors seat-map point))]
    (try
      (case (get-in seat-map point)
        \. \.
        \L (if (zero? (get neighbor-seats \# 0))
             \#
             \L)
        \# (if (<= 4 (get neighbor-seats \# 0))
             \L
             \#))
      (catch Object _
        (println "!!!!!" (get-in seat-map point))
        (throw (ex-info "Caught exception" {:neighbor-seats neighbor-seats
                                            :current-point {point (get-in seat-map point)}
                                            :seat-map seat-map}))))))

(defn step [seat-map]
  (let [max-rows (count seat-map)
        max-cols (count (first seat-map))]
    (->> (for [r (range max-rows)
               c (range max-cols)]
           (advance seat-map [r c]))
         (partition max-cols)
         (map #(apply str %))
         (into []))))


(def final-seat-map
  (loop [seat-map input-data]
    (let [result (step seat-map)]
      (if (= result seat-map)
        seat-map
        (recur result)))))

;; Part 1 solution
(count (filter #{\#} (apply str final-seat-map)))
;; => 2277




;; TESTING
(def test-data
  [["L.LL.LL.LL"
    "LLLLLLL.LL"
    "L.L.L..L.."
    "LLLL.LL.LL"
    "L.LL.LL.LL"
    "L.LLLLL.LL"
    "..L.L....."
    "LLLLLLLLLL"
    "L.LLLLLL.L"
    "L.LLLLL.LL"]
   ["#.##.##.##"
    "#######.##"
    "#.#.#..#.."
    "####.##.##"
    "#.##.##.##"
    "#.#####.##"
    "..#.#....."
    "##########"
    "#.######.#"
    "#.#####.##"]
   ["#.LL.L#.##"
    "#LLLLLL.L#"
    "L.L.L..L.."
    "#LLL.LL.L#"
    "#.LL.LL.LL"
    "#.LLLL#.##"
    "..L.L....."
    "#LLLLLLLL#"
    "#.LLLLLL.L"
    "#.#LLLL.##"]
   ["#.##.L#.##"
    "#L###LL.L#"
    "L.#.#..#.."
    "#L##.##.L#"
    "#.##.LL.LL"
    "#.###L#.##"
    "..#.#....."
    "#L######L#"
    "#.LL###L.L"
    "#.#L###.##"]
   ["#.#L.L#.##"
    "#LLL#LL.L#"
    "L.L.L..#.."
    "#LLL.##.L#"
    "#.LL.LL.LL"
    "#.LL#L#.##"
    "..L.L....."
    "#L#LLLL#L#"
    "#.LLLLLL.L"
    "#.#L#L#.##"]
   ["#.#L.L#.##"
    "#LLL#LL.L#"
    "L.#.L..#.."
    "#L##.##.L#"
    "#.#L.LL.LL"
    "#.#L#L#.##"
    "..L.L....."
    "#L#L##L#L#"
    "#.LLLLLL.L"
    "#.#L#L#.##"]])

(assert (= (take 6 test-data)
           (take 6 (iterate step (first test-data)))))


(def test-steady-state
  (last (take 6 (iterate step (first test-data)))))


(count (filter #{\#} (seq (apply str test-steady-state))))
;; => 37
