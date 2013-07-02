(ns tron.bots.dmcgillen
  (:require [tron.core :as tron]))

;; don-bot

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn valid-in-direction
  [look direction pos]
  (for [[x y] (rest (iterate direction pos))
        :while (not (look [x y]))]
    [x y]))

(defn don-bot
  [look {pos :pos}]
  (let [direction (max-key (fn [d] (count (valid-in-direction look d pos))) up down left right)]
    {:pos (direction pos)}))