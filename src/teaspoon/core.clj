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


(defprotocol ITourManager
  (add-city [t c])
  (get-city [t i])
  (number-of-cities [t]))

(defrecord TourManager [l]
  ITourManager
  (add-city [t c] (TourManager. (conj l c)))
  (get-city [t i] (nth l i))
  (number-of-cities [t] (count l)))

(defprotocol ITour
  (generate-individual [t tm n])
  (nth-city [t i])
  (set-city [t i c])
  (get-fitness [t])
  (get-distance [t])
  (get-tour-size [t])
  (contains-city [t c]))

(defrecord Tour [l]
  ITour
  (generate-individual [t tm n]
    (Tour. (shuffle
            (for [i (range n)]
                (get-city tm i)))))
  (nth-city [t i] (nth l i))
  (set-city [t i c] (Tour. (assoc l i c)))
  (get-fitness [t] (/ 1 (.get-distance t)))
  (get-distance [t]
    (reduce + 0 (map-indexed
                 (fn [i v]
                   (.get-distance v (get l (inc i))))
                 (butlast l))))
  (get-tour-size [t] (count l))
  (contains-city [t c] (some #(= c %) l)))
