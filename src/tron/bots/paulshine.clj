(ns tron.bots.paulshine
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

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn- neighbours [position]
  (vector
   (right position)
   (up position)
   (down position)
   (left position)))

(defn- valid-neighbours [look position]
  (let [nbhrs (neighbours position)]
    (for [coordinate nbhrs
          :when (not (look coordinate))]
      coordinate)
    ))

(defn- all-valid [look direction position]
  (take-while (complement look) (rest (iterate direction position))) )

(defn test
  [look pos]
  (let [down-cs (all-valid look down pos)
        up-cs (all-valid look up pos)
        left-cs (all-valid look left pos)
        right-cs (all-valid look right pos)]
    (val (first (sorted-map-by >
                 (count down-cs) down
                 (count up-cs) up
                 (count left-cs) left
                 (count right-cs) right)))))


(defn runme [look {pos :pos}]
  {:pos ((test look pos) pos)})
