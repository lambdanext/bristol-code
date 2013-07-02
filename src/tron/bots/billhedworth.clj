(ns tron.bots.billhedworth
  (:require [tron.core :as tron]))


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

(defn buzz [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn down-or-right [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))


(defn- neighbours [position]
  [(up position)
   (down position)
   (left position)
   (right position)])

(defn random-direction []
  (rand-nth [right left up down]))

(defn valid-neighbours [look position]
  (let [all-neighbours (neighbours position)]
    (for [coord all-neighbours
           :when (not (look coord))]
       coord)))

(defn valid-direction [look position direction]
  (take-while (complement look) (rest (iterate direction position))))

(defn move-random
  [look {pos :pos}]
   {:pos (rand-nth (valid-neighbours look pos))})

(defn move-new [look pos]   
  (let [all-up  (valid-direction look pos up)
        all-down (valid-direction look pos down)
        all-left (valid-direction look pos left)
        all-right (valid-direction look pos right)
        ;sortedmap (sorted-map-by > up all-up, down all-down)
        ;dir-map {up all-up down all-down left all-left right all-right}
        ] (val (first (sorted-map-by >
                              (count all-up) up
                              (count all-down) down
                              (count all-left) left
                              (count all-right) right
                              )))))

(defn move [look {pos :pos}]
   {:pos ((move-new look pos) pos)})