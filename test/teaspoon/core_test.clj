(ns teaspoon.core-test
  (:require [clojure.test :refer :all]
            [teaspoon.core :refer :all])
  (:import [teaspoon.core City]))

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
