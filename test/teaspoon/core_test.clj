(ns teaspoon.core-test
  (:require [clojure.test :refer :all]
            [teaspoon.core :refer :all])
  (:import [teaspoon.core City TourManager Tour]))

(deftest city-test
  (testing "Tests of the ICity interface."
    (let [c (City. 10 12)
          c1 (City. 11 13)]
      (is (= 10 (get-x c)))
      (is (= 12 (get-y c)))
      (is (= 11 (get-x c1)))
      (is (= 13 (get-y c1)))
      (is (= (Math/sqrt 2.0) (distance-to c c1)))
      )))

(deftest tour-manager-test
  (testing "Tests of the ITourManager interface."
    (let [t (TourManager. [])
          c (City. 10 12)
          c1 (City. 11 13)
          u (add-city t c)]
      (is (= 0 (number-of-cities t)))

      (is (= 1 (number-of-cities u)))
      (is (= 10 (get-x (get-city u 0))))
      (is (= 12 (get-y (get-city u 0))))
      )))

(deftest tour-test
  (testing "Tests the ITour interface."
    (let [cities [(City. 10 12) (City. 11 13)]
          tm (TourManager. cities)
          t (generate-individual (Tour. []) tm (number-of-cities tm)) ]
      (is (every? #(contains-city t %) cities))
      )))
