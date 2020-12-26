(ns day1)

(def expense-report
  (sort (read-string (slurp "day01.txt"))))

(defn combine-while [initial-val xs combine-fn pred]
  (for [x xs
        y initial-val
        :let [result (combine-fn y x)]
        :when (pred result)]
    result))

;; Part 1 solution
(combine-while (map vector expense-report)
               expense-report
               conj
               (fn [xs]
                 (= 2020 (apply + xs))))
;; => ([1632 388] [388 1632])

;; Part 2 solution
(let [combine (fn [iv pred]
                (combine-while iv expense-report conj pred))]
  (-> (map vector expense-report)
      (combine #(< (apply + %) 2020))
      (combine #(= 2020 (apply + %)))))
;; => ([1607 217 196]
;;     [217 1607 196]
;;     [1607 196 217]
;;     [196 1607 217]
;;     [217 196 1607]
;;     [196 217 1607])
