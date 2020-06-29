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
  ))

(deftest read-birth-data-test
  (testing "read-birth-data fail."
    (is (= (count (read-birth-data)) 63))
  ))

(deftest live-births-test
  (testing "live-births fail."
    (is (= (live-births (read-birth-data) 2009) 4131019))
    (is (= (live-births (read-birth-data) 2010) 4131019))
    (is (= (live-births (read-birth-data) 2008) 4131019))
    (is (= (live-births (read-birth-data) 1910) 2777000))
    (is (= (live-births (read-birth-data) 1900) 2777000))
  ))

(deftest read-age-histogram-test
  (testing "read-age-histogram fail."
    (is (= (count (read-age-histogram)) 21))
  ))
