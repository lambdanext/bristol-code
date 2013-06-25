(ns tron.bots.benjamin
  (:require [tron.core :as tron]))


(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])

(defn valid-moves [pos]
  [(left pos)
  (right pos)
  (up pos)
  (down pos)
  ])

(defn clockwise
  [look {pos :pos}]
  {:pos (first (remove look (valid-moves pos)))}
  )

;; launch bots
