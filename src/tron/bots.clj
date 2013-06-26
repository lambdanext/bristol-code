(ns tron.bots
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

(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

;; launch bots
(tron/run buzz down-or-right)