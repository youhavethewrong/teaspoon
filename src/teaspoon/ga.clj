(ns teaspoon.ga
  (:require [teaspoon.core :refer :all])
  (:import [teaspoon.core City Population Tour TourManager]))

(def mutation-rate 0.015)
(def tournament-size 5)
(def elitism true)
(def elitism-offset (atom 0))

(defn get-empty-population
  [n]
  (Population.
   (vec
    (java.util.ArrayList. n))))

(defn get-empty-tour
  [n]
  (Tour.
   (vec
    (java.util.ArrayList. n))))

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
    (Tour. ct)))

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
    (let [i (.intValue (* (Math/random) (get-tour-size t)))
          j (.intValue (* (Math/random) (get-tour-size t)))
          city0 (nth-city t i)
          city1 (nth-city t j)
          t' (set-city t i city1)
          t' (set-city t' j city0)]
      t')
    t))

(defn mutate-population
  [p]
  (Population. (vec (cons (get-tour p 0)
                          (for [i (range @elitism-offset (population-size p))]
                            (mutate (get-tour p i)))))))

(defn evolve-population
  [p]
  (->> (new-population p)
       (crossover-population p)
       (mutate-population)))

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
