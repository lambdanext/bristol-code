(ns tron.bots.darley
  (:require [tron.core :as tron]))

(defn empty-look
  "A mock look function which just checks for the arena
   boundaries."
  [pos]
  (when-not (tron/valid-pos? pos) :wall))

(defn mock-look
  "Returns a mock look function which checks for the arena
   boundaries and the specified occupied positions."
  [& occupied-poses]  (let [occupied-poses (set occupied-poses)]
    (fn [pos]
      (or (occupied-poses pos)
        (when-not (tron/valid-pos? pos) :wall)))))

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn neighbours [pos]
  [(up pos)
   (down pos)
   (left pos)
   (right pos)])

(defn valid-neighbours [look pos]
  (let [all-neighbours (neighbours pos)]
    (filter (complement look) all-neighbours)))

(defn all-valid [direction look pos]
  (take-while (complement look) (rest (iterate direction pos))))

(defn find-furthest [look pos]
  (let [up-c    (all-valid up look pos)
        down-c  (all-valid down look pos)
        left-c  (all-valid left look pos)
        right-c (all-valid right look pos)]
    (val (first (sorted-map-by >
                 (count up-c) up
                 (count down-c) down
                 (count left-c) left
                 (count right-c) right)))))

(defn random-direction [look pos]
  (let [valid-directions (valid-neighbours look pos)
        direction (when (seq valid-directions) (rand-nth valid-directions))]
    direction))

(defn pick-direction [look pos]
  (let [method (rand-int 6)]
    (if (<= method 4)
      ((find-furthest look pos) pos)
      (random-direction look pos))))

(defn think
  [look {pos :pos}]
  {:pos (pick-direction look pos)})
