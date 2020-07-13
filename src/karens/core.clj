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

(defn prob-name-given-birth-decade-and-sex
  "Give the probability that someone has a given name knowing only the decade of their birth and sex of as evidence."
  [name-list birth-totals birth-name decade sex]
  (let [name-count (:count (first (filter #(and (= sex (:sex %))
                                                (= birth-name (:name %))
                                                (= decade (:decade %))) name-list)))
        entry (first (filter #(= decade (:decade %)) birth-totals))
        total ((if (= "male" sex) :male-count :female-count) entry)
        ]
    (if (= nil name-count)
      0
      (float (/ name-count total)))))

(defn prob-birth-name
  "Give the probability of someone having a given birth name knowing only their sex."
  [name-list birth-totals age-histogram birth-name sex]
  (let [decades (range 1900 2020 10)
        ; find the total number of folks with the name and sex, divide by total number of folks with that sex
        sex-total (reduce + (map (if (= sex "male") :male-count :female-count) age-histogram))
        ; total number of folks with given name and sex
          ; survivors with name = survivors in age group * (folks born with name in that decade/folks born in that decade)
        name-counts (map :count (filter #(and (= (:sex %) sex)
                                              (= (:name %) birth-name)) name-list))
        survivor-counts (for [i (range (count name-counts))]
                          (* (reduce + (map #((if (= sex "male") :male-count :female-count) %) (take 2 (drop i age-histogram))))
                             (/ (nth name-counts i)
                                ((if (= sex "male") :male-count :female-count) (nth birth-totals i)))))
        ]
    (float (/ (reduce + survivor-counts) sex-total))
    ))

(defn prob-birth-decade-given-name-and-sex
  "Compute the probability someone has a given birth decade given only their name and sex.  Account for whether they are alive or dead (i.e., age histograms)."
  [name-list birth-totals age-histogram decade birth-name sex]
  (let [; by a Bayesian calculation, P(decade|name) = P(name|decade)*P(decade)/P(name)
        p-name-given-decade (prob-name-given-birth-decade-and-sex name-list birth-totals birth-name decade sex)
        p-decade (prob-birth-decade age-histogram decade sex)
        p-name (prob-birth-name name-list birth-totals age-histogram birth-name sex)
        ]
    (if (= p-name 0.0)
      "Insufficient data."
      (float (/ (* p-name-given-decade p-decade) p-name)))))
