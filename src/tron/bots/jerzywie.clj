(ns tron.bots.jerzywie
  (:require [tron.core :as tron]))

;; INCLUDE THE NAME OF THE BOTT FN AT THE TOP

(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])

(def dirs [left right up down])


(defn neighbours
  [p]
  (vector (up p) (down p) (left p) (right p)))

(defn valid-neighbours
  [look p]
  (remove look (neighbours p)))

(defn valid-to
  [look pos direction]
  (for [testp (rest (iterate direction pos)) :while (not (look testp))]
    testp))


(defn jerzy-bot
  [look {pos :pos hist :hist}]
  {:pos ( (key (last (sort-by last (zipmap dirs (map #(count (valid-to look pos %)) dirs))))) pos)
   :hist (conj hist pos)})

