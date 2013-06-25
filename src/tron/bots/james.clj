(ns tron.bots.james
  (:require [tron.core :as tron]))
(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn all-moves
  [pos]
  [(right pos)
   (left pos)
   (up pos)
   (down pos)])

(defn valid-moves [look pos]
  (remove look (all-moves pos)))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn distance [[ox oy] [cx cy]]
  (+ (Math/pow ( - ox cx) 2) (Math/pow ( - oy cy) 2)))

(defn spiral-buzz [look {pos :pos o :origin :as state}]
  (let [origin (or o pos)
        closest-move (apply min-key (partial distance origin) (valid-moves look pos))]
      {:pos closest-move :origin origin}))





;; launch bots
