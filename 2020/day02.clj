(ns day2)


(def day2-parsed
  (with-open [r (clojure.java.io/reader "day02.txt")]
    (doall (line-seq r))))

(defn valid? [pw]
  #"^(?<lo>.*)-(?<hi>.*)\s(?<c>): (?<s>.*)$")

(defn parse-line [s]
  (let [matcher (re-matcher #"(?<lo>.*)-(?<hi>.*)\s+(?<c>.):\s+(?<s>.*)$" s)]
    (-> (zipmap [:lo :hi :c :password] (drop 1 (re-find matcher)))
        (update :lo (fn [x] (Integer/parseInt x)))
        (update :hi (fn [x] (Integer/parseInt x)))
        (update :c  (fn [x] (first (char-array x)))))))

(defn valid? [{:keys [lo hi c password] :as entry}]
  (<= lo (get (frequencies password) c -1) hi))

;; Part 1 solution
(count (filter valid? (map parse-line day2-parsed)))
;; => 410

(defn valid-2? [{:keys [lo hi c password] :as entry}]
  (->> (set [(= c (get (char-array password) (dec lo)))
             (= c (get (char-array password) (dec hi)))])
       count
       (= 2)))

;; Part 2 solution
(count (filter valid-2? (map parse-line day2-parsed)))
;; => 694
