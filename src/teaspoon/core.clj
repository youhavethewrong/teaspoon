(ns teaspoon.core)

(defprotocol ICity
  (get-x [c])
  (get-y [c])
  (distance-to [c c1]))

(defrecord City [x y]
  ICity
  (get-x [c] x)
  (get-y [c] y)
  (distance-to [c c1]
    (let [x-dist (Math/abs (- (get-x c) (get-x c1)))
          y-dist (Math/abs (- (get-y c) (get-y c1)))]
      (Math/sqrt (+ (* x-dist x-dist) (* y-dist y-dist))))))
