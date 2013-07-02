(ns tron.bots.iansugar
  (:require [tron.core :as tron]))

;; georgina

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

(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn neighbours [[x y]]
  "Gets neighbour position"
  [[x (- y 1)] 
   [x (+ y 1)]
   [(- x 1) y]
   [(+ x 1) y] 
   ])

(defn valid-neighbours [look pos]
  "Gets valid neighbours"
  (remove look (neighbours pos)))

(defn valid-positions-in-direction [direction look pos]
  "Gets all valid positions in a direction from position"
  (take-while (comp not look) (iterate direction pos)))

(defn distance-in-direction [direction look pos]
  "Gets distance in direction before invalid position"
  (count (valid-positions-in-direction direction look (direction pos))))

(defn georgina
  [look {pos :pos lifetime :lifetime :or {lifetime 0}}]
  (let [direction (max-key #(distance-in-direction % look pos) up down left right)]
    {:pos (direction pos) :direction direction :lifetime (inc lifetime)}))
