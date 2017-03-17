(ns teaspoon.sa
  (:require
   #?(:clj [teaspoon.core :as c]
      :cljs [teaspoon.core :as c :refer [Tour]]))
  #?(:clj
     (:import [teaspoon.core Tour])))

(def cooling-rate 0.003)

(defn acceptance-probability
  [e e' t]
  (letfn [(exp [v] #?(:clj (Math/exp v)
                      :cljs (.exp js/Math v)))]
    (if (< e' e)
      1.0
      (exp (/ (- e e') t)))))

(defn find-solution
  "Given an initial TourManager tm and initial temperature t0 find an optimal
   route for the tour."
  [tm t0]
  (loop [best-tour (c/generate-individual (Tour. []) tm (c/number-of-cities tm))
         current-tour best-tour
         temp t0]
    (if (< temp 1)
      best-tour
      (let [to-int #?(:clj (fn [x] (.intValue x))
                      :cljs (fn [x] (.floor js/Math x)))
            u (Tour. (:l current-tour))
            p0 (to-int (* (rand) (c/get-tour-size u)))
            p1 (to-int (* (rand) (c/get-tour-size u)))
            c0 (c/nth-city u p0)
            c1 (c/nth-city u p1)
            u (c/set-city u p0 c1)
            u (c/set-city u p1 c0)
            e (c/get-distance current-tour)
            e' (c/get-distance u)
            current-tour (if (> (acceptance-probability e e' temp) (rand))
                           u
                           current-tour)
            best-tour (if (< (c/get-distance current-tour)
                             (c/get-distance best-tour))
                        current-tour
                        best-tour)]
        (recur best-tour current-tour (* temp (- 1 cooling-rate)))))))
