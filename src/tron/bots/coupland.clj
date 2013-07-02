(ns tron.bots.coupland
  (:require [tron.core :as tron]))

;;bot func: wallhugger

(defn empty-look
  "A mock look function which just checks for the arena
   boundaries."
  [pos]
  (when-not (tron/valid-pos? pos) :wall))

(defn mock-look
  "Returns a mock look function which checks for the arena
   boundaries and the specified occupied positions."
  [& occupied-poses]
  (let [occupied-poses (set occupied-poses)]
    (fn [pos]
      (or (occupied-poses pos)
        (when-not (tron/valid-pos? pos) :wall)))))

(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn pos-to-wall
  [look direction pos]
  (take-while #(not (look %)) (iterate direction (direction pos))))

(defn spaces-til-wall
  [look pos direction]
  (count (pos-to-wall look direction pos)))

(def vec? vector?)

(defn closest-wall-pos
  [look pos]
  (let [r (partial spaces-til-wall look pos)]
    (first
     (sort
      (fn [a b]
        (let [raw (compare (second a)
                           (second b))]
          (if (vec? (nth 2 a))
            1
            raw)))
      (map
       #(conj % (look (first %)))
       [[(up pos) (r up)]
        [(down pos) (r down)]
        [(left pos) (r left)]
        [(right pos) (r right)]])))))

(defn wallhugger
  "Wall hugger"
  [look {pos :pos}]
  (let [[d dd] (closest-wall-pos look pos)]
    (prn "D-> " d)
    {:pos d}))

