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
  (get-fittest [p] (:tour
                    (first
                     (sort-by :fitness
                              (map (fn [tour]
                                     {:fitness (get-fitness tour)
                                      :tour tour})
                                   l)))))
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
                                 (range start end)
                                 (range end start))))]
    (Tour. (vec
            (for [i (range t1-size)]
              (if (contains? r i)
                (nth-city t1 i)
                (nth-city t2 i)))))))

(defn tournament-selection
  [p]
  (let [p1 (Population.
            (vec
             (for [i (range @elitism-offset tournament-size)]
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
          (swap! elitism-offset inc)
          (save-tour p1 0 (get-fittest p)))
        p1)))

(defn crossover-population
  [p p1]
  (Population.
   (vec
    (for [i (range @elitism-offset (population-size p1))]
      (let [t1 (tournament-selection p)
            t2 (tournament-selection p)]
        (crossover t1 t2))))))

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
        m (mutate-population c)]
    m))
