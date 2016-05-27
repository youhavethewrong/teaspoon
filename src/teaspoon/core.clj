(ns teaspoon.core
  (:require             [clojure.pprint :refer [pprint]]))

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
  (get-fitness [t] (/ 1 (get-distance t)))
  (get-distance [t]
    (reduce + 0 (map-indexed
                 (fn [i v]
                   (distance-to v (get l (inc i))))
                 (butlast l))))
  (get-tour-size [t] (count l))
  (contains-city [t c] (some #(= c %) l)))

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

;; -- GA --

(def mutation-rate 0.015)
(def tournament-size 5)
(def elitism true)
(def elitism-offset (atom 0))

(defn get-empty-population
  [n]
  (Population.
   (vec (for [i (range n)]
          nil))))

(defn get-empty-tour
  [n]
  (Tour.
   (vec (for [i (range n)]
          nil))))

(defn crossover
  [t1 t2]
  (let [t1-size (get-tour-size t1)
        start (* (Math/random) t1-size)
        end (* (Math/random) t1-size)
        ordered? (< start end)
        r (set
           (map #(.intValue %) (if ordered?
                                 (range start (inc end))
                                 (range end (inc start)))))
        ct (vec
            (for [i (range t1-size)]
              (if (contains? r i)
                (nth-city t1 i)
                (nth-city t2 i))))]
    (Tour. ct)
))

(defn tournament-selection
  [p]
  (let [p1 (Population.
            (vec
             (for [i (range tournament-size)]
               (get-tour p (* (Math/random) (population-size p))))))]
    (get-fittest p1)))

(defn new-population
  [p]
  (let [p1 (get-empty-population (population-size p))]
      (if elitism
        (do
          (reset! elitism-offset 1)
          (save-tour p1 0 (get-fittest p)))
        p1)))

(defn crossover-population
  [p p1]
  (let [v (for [i (range @elitism-offset (population-size p))]
            (let [t1 (tournament-selection p)
                  t2 (tournament-selection p)]
              (crossover t1 t2)))]
    (Population. (vec (cons (get-tour p 0) v )))))

(defn mutate
  [t]
  (if (< (Math/random) mutation-rate)
    (let [shv (shuffle (:l t))]
      (Tour. shv))
    t))

(defn mutate-population
  [p]
  (let [v (for [i (range @elitism-offset (population-size p))]
          (mutate (get-tour p i)))]
    (Population. (vec (cons (get-tour p 0) v)))))

(defn evolve-population
  [p]
  (let [n (new-population p)
        c (crossover-population p n)
        m (mutate-population c)]
    m))

(defn find-solution
  [tm s n]
  (reset! elitism-offset 0)
  (loop [p (initialize (Population. []) tm s)
         c 0]
    (println "Generation" c)
    (println "Distance" (get-distance (get-fittest p)))
    (if (>= c n)
      p
      (recur (evolve-population p) (inc c)))))
