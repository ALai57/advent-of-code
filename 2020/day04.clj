(ns day4
  (:require [clojure.spec.alpha :as s]))


(def split-input
  (clojure.string/split (slurp "day04.txt") #"\n\n"))

(def parsed-input
  (let [x (map (fn ([s] (clojure.string/split s #"\s")))
               split-input)]
    (for [v x]
      (->> v
           (map (fn [s] (clojure.string/split s #":")))
           (into {})
           clojure.walk/keywordize-keys))))

(defn my-contains? [ks m]
  (= (count ks)
     (count (select-keys m ks))))

;; Part 1 solution
(count (filter (partial my-contains? [:byr :iyr :eyr :hgt :hcl :ecl :pid])
               parsed-input))
;; => 216


;; PART 2
(defn valid-year? [mn mx s]
  (and (re-find #"^[0-9]{4}$" s)
       (<= mn (Integer/parseInt s) mx)))

(defn valid-height? [s]
  (let [[_ ht unit] (re-find #"^(?<val>[0-9]*)(?<unit>cm|in)$" s)]
    (case unit
      "cm" (<= 150 (Integer/parseInt ht) 193)
      "in" (<= 59 (Integer/parseInt ht) 76)
      nil)))

(defn valid-hair? [s]
  (re-find #"^\#[0-9a-f]{6}$" s))

(defn valid-eyes? [s]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))

(defn valid-pid? [s]
  (re-find #"^\d{9}$" s))

(s/def ::byr (partial valid-year? 1920 2002))
(s/def ::iyr (partial valid-year? 2010 2020))
(s/def ::eyr (partial valid-year? 2020 2030))
(s/def ::hgt valid-height?)
(s/def ::hcl valid-hair?)
(s/def ::ecl valid-eyes?)
(s/def ::pid valid-pid?)

(s/def ::passport (s/keys :req-un [::byr
                                   ::iyr
                                   ::eyr
                                   ::hgt
                                   ::hcl
                                   ::ecl
                                   ::pid]))

;; Part 2 solution
(count (filter (partial s/valid? ::passport)
               parsed-input))
;; => 150


;; Test cases
;;
;;
(defn vec->passport [xs]
  (let [x (map (fn ([s] (clojure.string/split s #"\s")))
               xs)]
    (for [v x]
      (->> v
           (map (fn [s] (clojure.string/split s #":")))
           (into {})
           clojure.walk/keywordize-keys))))

(for [x [["eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"]
         ["iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946 "]
         ["hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"]
         ["hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"]]]
  (assert (false? (s/valid? ::passport (first (vec->passport x))))))

(for [x [["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"]
         ["eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"]
         ["hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"]
         ["iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]]]
  (assert (true? (s/valid? ::passport (first (vec->passport x))))))
