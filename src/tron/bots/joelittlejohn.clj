(ns tron.bots.joelittlejohn
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

(defn up [[x y]]
  [x (dec y)])

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn neighbours [pos]
  ((juxt up down left right) pos))

(defn valid-neighbours [look pos]
  {:post [(not-any? look %)]}
  (remove look (neighbours pos)))

(defn valid-moves [look pos moves]
  (remove #(look (% pos)) moves))

(def preferred-moves {up [left up right]
                      left [down left up]
                      down [right down left]
                      right [up right down]})

(defn joebot [look {pos :pos last-move :last-move :or {last-move left}}]
  (let [m (first (valid-moves look pos (preferred-moves last-move)))]
    {:pos (m pos)
     :last-move m}))