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

(defn mutate
  [t]
  (for [i (range (get-tour-size t))]
    (if (< (Math/random) mutation-rate)
      (let [j (.intValue (* (Math/random) (get-tour-size t)))
            c1 (nth-city t i)
            c2 (nth-city t j)
            t (set-city t i c2)
            t (set-city t j c1)]
        t)
      t)))

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
  (let [v (vec
           (for [i (range @elitism-offset (population-size p))]
             (let [t1 (tournament-selection p)
                   t2 (tournament-selection p)]
               (crossover t1 t2))))]
    (Population. (cons (get-tour p 0) v ))))

(defn mutate-population
  [p]
  (Population.
   (flatten
    (vec
     (for [i (range @elitism-offset (population-size p))]
       (mutate (get-tour p i)))))))

(defn evolve-population
  [p]
  (let [n (new-population p)
        c (crossover-population p n)
;;        mutate is horribly slow and probably broken
;;        m (mutate-population c)
        ]
    c))



(comment

    (let [c1 (City. 60 200)
          c2 (City. 180 200)
          c3 (City. 80 180)
          c4 (City. 140 180)
          c5 (City. 20 160)
          c6 (City. 100 160)
          c7 (City. 200 160)
          c8 (City. 140 140)
          c9 (City. 40 120)
          c10 (City. 100 120)
          c11 (City. 180 100)
          c12 (City. 60 80)
          c13 (City. 120 80)
          c14 (City. 180 60)
          c15 (City. 20 40)
          c16 (City. 100 40)
          c17 (City. 200 40)
          c18 (City. 20 20)
          c19 (City. 60 20)
          c20 (City. 160 20)
          tm (TourManager. [c1  c2  c3  c4
                            c5  c6  c7  c8
                            c9  c10 c11 c12
                            c13 c14 c15 c16
                            c17 c18 c19 c20])
          ]
      (reset! elitism-offset 0)
      (loop [p (initialize (Population. []) tm 50)
             c 0]
        (println "Loop" c)
        (println "Elitism" @elitism-offset)
        (println "Distance" (get-distance (get-fittest p)))
        (if (>= c 100)
          p
          (recur (evolve-population p) (inc c))))

      )
    )
