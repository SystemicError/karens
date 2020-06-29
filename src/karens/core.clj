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
  "Reads live birth data"
  []
  (let [filename "data/total_births"
        text (slurp filename)
        decommaed (str/replace text #"," "")
        parse-line (fn [line]
                     (let [fields (str/split line #"\s+")]
                       {:year (read-string (first fields))
                        :count (read-string (nth fields 1))
                        }))
        lines (str/split decommaed #"\n")
        entries (map parse-line lines)
        ]
    entries))

(defn live-births
  "Give the number of live births in a requested year, interpolating if necessary."
  [birth-data year]
  ; uses nearest interpolation (lazy, I know)
  (:count (apply min-key #(Math/abs (- (:year %) year)) birth-data)))

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

