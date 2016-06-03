(ns teaspoon.sa
  (:require [teaspoon.core :refer :all])
  (:import [teaspoon.core City Tour TourManager]))

(def cooling-rate 0.003)

(defn acceptance-probability
  [e e' t]
  (if (< e' e)
    1.0
    (Math/exp (/ (- e e') t))))

(defn find-solution
  "Given an initial TourManager tm and initial temperature t0 find an optimal
   route for the tour."
  [tm t0]
  (loop [best-tour (generate-individual (Tour. []) tm (number-of-cities tm))
         current-tour best-tour
         temp t0]
    (println "Distance" (get-distance best-tour))
    (if (< temp 1)
      best-tour
      (let [u (Tour. (:l current-tour))
            p0 (.intValue (* (Math/random) (get-tour-size u)))
            p1 (.intValue (* (Math/random) (get-tour-size u)))
            c0 (nth-city u p0)
            c1 (nth-city u p1)
            u (set-city u p0 c1)
            u (set-city u p1 c0)
            e (get-distance current-tour)
            e' (get-distance u)
            current-tour (if (> (acceptance-probability e e' temp) (Math/random))
                           u
                           current-tour)
            best-tour (if (< (get-distance current-tour) (get-distance best-tour))
                        current-tour
                        best-tour)]
        (recur best-tour current-tour (* temp (- 1 cooling-rate)))))))
