(ns karens.core
  (:require [clojure.string :as str]))

(defn read-name-list
  "Reads in SSA data on 200 most popular names by sex and decade."
  [decade]
  (let [filename (str "data/" decade "s")
        text (slurp filename)
        decommaed (str/replace text #"," "")
        parse-line (fn [line]
                     (let [fields (str/split line #"\s+")]
                       (list {:sex "male"
                              :name (nth fields 1)
                              :count (read-string (nth fields 2))
                              :decade decade
                              }
                             {:sex "female"
                              :name (nth fields 3)
                              :count (read-string (nth fields 4))
                              :decade decade
                              })))
        lines (str/split decommaed #"\n")
        entries (apply concat (map parse-line lines))
        ]
    entries))

(defn read-name-lists
  "Combines data from all SSA decades on most popular names."
  []
  (apply concat (map read-name-list (range 1900 2020 10))))

(defn read-birth-data
  "Reads SSA birth totals"
  []
  (let [filename "data/total_births"
        text (slurp filename)
        parse-line (fn [line]
                     (let [fields (str/split line #"\s+")]
                       {:decade (read-string (first fields))
                        :male-count (read-string (nth fields 1))
                        :female-count (read-string (nth fields 2))
                        }))
        lines (str/split text #"\n")
        entries (map parse-line lines)
        ]
    entries))

(defn read-age-histogram
  "Reads age-histogram"
  []
  (let [filename "data/age_histogram.csv"
        text (slurp filename)
        parse-line (fn [line]
                     (let [fields (str/split line #",")]
                       {:age (first fields)
                        :male-count (read-string (nth fields 1))
                        :female-count (read-string (nth fields 2))
                        }))
        lines (str/split text #"\n")
        entries (map parse-line lines)
        ]
    entries))

(defn prob-birth-decade
  "Give the probability that someone was born in a given decade, using only their sex as evidence."
  [age-histogram decade sex]
  (let [; add up number of people with their decade and sex in the histogram 
        index (* 2 (- 201 (/ decade 10)))
        pentades (take 2 (drop index age-histogram))
        decaders (if (= sex "male")
                   (+ (:male-count (first pentades))
                      (if (> decade 1910)
                        (:male-count (nth pentades 1))
                        0))
                   (+ (:female-count (first pentades))
                      (if (> decade 1910)
                        (:female-count (nth pentades 1))
                        0)))
        ; divide by the total number of people in the histogram (US population)
        total (reduce + (map #(if (= sex "male")
                                (:male-count %)
                                (:female-count %))
                             age-histogram))
        ]
    (float (/ decaders total))))

(defn prob-name-given-decade
  "Give the probability that someone has a given name knowing only the decade of their birth and sex of as evidence."
  [name-list birth-totals decade sex]
  (let [name-count (:count (first (filter #(and (= sex (:sex %))
                                                (= decade (:decade %))) name-list)))
        entry (first (filter #(= decade (:decade %)) birth-totals))
        total ((if (= "male" sex) :male :female) entry)
        ]
    (/ name-count total)))
