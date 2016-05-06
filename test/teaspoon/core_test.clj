(ns teaspoon.core-test
  (:require [clojure.test :refer :all]
            [teaspoon.core :refer :all])
  (:import [teaspoon.core City Population Tour TourManager]))

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
    (let [twin-cities [(City. 10 12) (City. 11 13)]
          tri-cities [(City. 3 4) (City. 2 5) (City. 1 6)]
          twin-m (TourManager. twin-cities)
          tri-m (TourManager. tri-cities)
          tt (generate-individual (Tour. []) twin-m (number-of-cities twin-m))
          ti (generate-individual (Tour. []) tri-m (number-of-cities tri-m))]
      (is (every? #(contains-city tt %) twin-cities))
      (is (= (Math/sqrt 2.0) (get-distance tt)))
      (is (not-every? #(contains-city tt %) tri-cities))
      (is (every? #(contains-city ti %) tri-cities))
      (is (< (Math/sqrt 2.0) (get-distance ti)))
      )))

(deftest population-test
  (testing "Tests the IPopulation interface."
    (let [c (City. 2 6)
          c1 (City. 9 11)
          c2 (City. 10 4)
          cities [c c1 c2]
          tm (TourManager. cities)
          t (Tour. cities)
          t1 (Tour. [c c2 c1])
          t2 (Tour. [c1 c c2])
          p (Population. [])
          p1 (save-tour p 0 t)
          p2 (save-tour p1 1 t1)
          p3 (save-tour p2 2 t2)]
      (is (= 0 (population-size p)))
      (is (= 1 (population-size p1)))
      (is (= 2 (population-size p2)))
      (is (= 3 (population-size p3)))
      (is (= t (get-tour p3 0)))
      (is (= t1 (get-tour p3 1)))
      (is (= t2 (get-tour p3 2)))
      (is (< (get-fitness (get-fittest p3)) 0.6))
      )))
