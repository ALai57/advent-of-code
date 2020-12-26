(ns day8)


(def input-data
  (vec (with-open [r (clojure.java.io/reader "day08.txt")]
         (doall (line-seq r)))))


(defn parse-instruction [s line-num]
  (let [[_ instruction val] (re-find #"(acc|jmp|nop)\s\+*(\-*\d+)"
                                     (get s line-num))]
    [instruction (Integer/parseInt val)]))

(parse-instruction input-data 1)
;; => ["jmp" 37]

(defn step [program {:keys [acc-ctr jmp-ctr lines chain] :as s}]
  (let [[instruction v]
        (if (<= (count program) jmp-ctr)
          (throw (ex-info "Attempted to access non-existent line"
                          {:jmp-ctr jmp-ctr
                           :acc-ctr acc-ctr}))
          (parse-instruction program jmp-ctr))]
    (try
      (assert (= 0 (get lines jmp-ctr)))
      (case instruction
        "acc" (-> s
                  (update-in [:chain] conj jmp-ctr)
                  (update-in [:acc-ctr] + v)
                  (update-in [:lines jmp-ctr] inc)
                  (update-in [:jmp-ctr] inc))
        "jmp" (-> s
                  (update-in [:chain] conj jmp-ctr)
                  (update-in [:lines jmp-ctr] inc)
                  (update-in [:jmp-ctr] + v))
        "nop" (-> s
                  (update-in [:chain] conj jmp-ctr)
                  (update-in [:lines jmp-ctr] inc)
                  (update-in [:jmp-ctr] inc)))
      (catch Object o
        (throw (ex-info "Circular state detected"
                        {:jmp-ctr jmp-ctr
                         :acc-ctr acc-ctr
                         :chain chain
                         :data o}))))))

;; Part 1
(reduce (fn [acc _]
          (try
            (step input-data acc)
            (catch Object e
              (reduced {:reason (ex-message e)
                        :line (:jmp-ctr (ex-data e))}))))
        {:acc-ctr 0
         :jmp-ctr 0
         :lines (zipmap (range (count input-data)) (repeat 0))}
        (range 10000))
;; => {:reason "Circular state detected", :line 458}



;; Part 2
(defn non-existent-line? [s]
  (clojure.string/includes? s "Attempted to access non-existent line"))

(defn runner [program]
  (try
    (loop [state {:acc-ctr 0
                  :jmp-ctr 0
                  :lines (zipmap (range (count program)) (repeat 0))}]
      (recur (step program state)))
    (catch Object e
      (cond
        (non-existent-line? (ex-message e)) {:reason (ex-message e)
                                             :line (:jmp-ctr (ex-data e))
                                             :acc-ctr (:acc-ctr (ex-data e))}
        :else nil))))

(def alternate-instructions
  (->> (for [s input-data]
         (cond
           (re-find #"nop" s) (clojure.string/replace s #"nop" "jmp")
           (re-find #"jmp" s) (clojure.string/replace s #"jmp" "nop")))
       (map-indexed #(vector %1 %2))
       (filter second)))


(def results
  (reduce (fn [acc [idx ins]]
            (let [program (assoc-in input-data [idx] ins)]
              (if-let [result (runner program)]
                (conj acc (merge result {:modified-line [idx ins]}))
                acc)))
          []
          alternate-instructions))

(doto results
  clojure.pprint/pprint)
;; => [{:reason "Attempted to access non-existent line",
;;      :line 636,
;;      :acc-ctr 2212,
;;      :modified-line [413 "nop -179"]}]
