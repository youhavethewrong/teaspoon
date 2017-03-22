(ns teaspoon.core
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]))

(defprotocol ICity
  (get-x [c])
  (get-y [c])
  (distance-to [c c1]))

(defrecord City [x y]
  ICity
  (get-x [c] x)
  (get-y [c] y)
  (distance-to [c c1]
    (let [abs (fn [v] #?(:clj (Math/abs v)
                         :cljs (.abs js/Math v)))
          sqrt (fn [v] #?(:clj (Math/sqrt v)
                          :cljs (.sqrt js/Math v)))
          x-dist (abs (- (get-x c) (get-x c1)))
          y-dist (abs (- (get-y c) (get-y c1)))]
      (sqrt (+ (* x-dist x-dist) (* y-dist y-dist))))))

(s/def ::y-coord integer?)
(s/def ::x-coord integer?)
(s/def ::city
  (s/with-gen #(instance? City %)
    (fn [] (gen/fmap
            (fn [[x y n]] (->City x y))
            (s/gen (s/tuple ::x-coord ::y-coord))))))

(defprotocol ITourManager
  (add-city [t c])
  (get-city [t i])
  (number-of-cities [t]))

(defrecord TourManager [l]
  ITourManager
  (add-city [t c] (TourManager. (conj l c)))
  (get-city [t i] (nth l i))
  (number-of-cities [t] (count l)))

(s/def ::tour-manager
  (s/with-gen #(instance? TourManager %)
    (fn [] (gen/fmap
            #(TourManager. %)
            (s/gen (s/coll-of ::city :distinct true :min-count 3))))))

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
                (.get-city tm i)))))
  (nth-city [t i] (nth l i))
  (set-city [t i c] (Tour. (assoc l i c)))
  (get-fitness [t] (/ 1 (.get-distance t)))
  (get-distance [t]
    (reduce + 0 (map-indexed
                 (fn [i v]
                   (.distance-to v (nth-city t (inc i))))
                 (butlast l))))
  (get-tour-size [t] (count l))
  (contains-city [t c] (some #(= c %) l)))

(s/def ::tour (s/coll-of ::city :kind vector? :distinct true :min-count 3))

(defprotocol IPopulation
  (initialize [p tm n])
  (save-tour [p i t])
  (get-tour [p i])
  (get-fittest [p])
  (population-size [p]))

(defrecord Population [l]
  IPopulation
  (initialize [p tm n]
    (Population.
     (vec
      (for [i (range n)]
        (generate-individual
         (Tour. [])
         tm
         (number-of-cities tm))))))
  (save-tour [p i t] (Population. (assoc l i t)))
  (get-tour [p i] (nth l i))
  (get-fittest [p] (let [b (sort-by :fitness
                                    (map (fn [tour]
                                           {:fitness (get-fitness tour)
                                            :tour tour})
                                         l))]
                     (Tour.
                      (:l (:tour (last b))))))

  (population-size [p] (count l)))

(s/def ::population (s/coll-of ::tour))
