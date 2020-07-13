(ns karens.core-test
  (:require [clojure.test :refer :all]
            [karens.core :refer :all]))

(deftest read-name-list-test
  (testing "read-name-lists fail."
    (is (= (count (read-name-list 1900)) 400))
    (is (= (first (read-name-list 1910)) {:name "John" :count 376318 :sex "male" :decade 1910}))
  ))

(deftest read-name-lists-test
  (testing "read-name-lists fail."
    (is (= (count (read-name-lists)) 4800))
;    (println (str "Name lists:\n" (into [] (read-name-lists))))
  ))

(deftest read-birth-data-test
  (testing "read-birth-data fail."
    (is (= (count (read-birth-data)) 12))
    (is (= (:female-count (last (read-birth-data))) 17316359))
  ))

(deftest read-age-histogram-test
  (testing "read-age-histogram fail."
    (is (= (count (read-age-histogram)) 21))
  ))

(deftest prob-birth-decade-test
  (testing "prob-birth-decade fail."
    (let [histogram (read-age-histogram)
          m-probs (map #(prob-birth-decade histogram % "male") (range 1910 2020 10))
          f-probs (map #(prob-birth-decade histogram % "female") (range 1910 2020 10))
          ]
      (is (< 0 (first m-probs)))
      (is (> 1 (first m-probs)))
      (is (> 0.0001 (- 1 (reduce + m-probs))))
      (is (> 0.0001 (- 1 (reduce + f-probs))))
      )))

(deftest prob-name-given-birth-decade-and-sex-test
  (testing "prob-name-given-birth-decade-and-sex fail."
    (let [births (read-birth-data)
          names (read-name-lists)]
      (is (= (prob-name-given-birth-decade-and-sex names births "John" 1990 "male")
             (float (/ 240069 20550214)))))
    ))

(deftest prob-birth-decade-given-name-and-sex-test
  (testing "prob-birth-decade-given-name-and-sex fail."
    (let [births (read-birth-data)
          names (read-name-lists)
          age-histogram (read-age-histogram)]
      (println (str "P(1950s|'Karen',f)=" (prob-birth-decade-given-name-and-sex births names age-histogram 1950 "John" "male")))
      )))
