(ns tron.bots.mary
  (:require [tron.core :as tron]))

(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])


(defn down [[x y]]
  [x (inc y)])






(defn top-left? [[x y]]
  (and (> 30 x) (> 30 y))
  )
(defn top-right? [[x y]]
  (and (< 30 x) (> 30 y))
  )
(defn bottom-left? [[x y]]
  (and (> 30 x) (< 30 y))
  )
(defn bottom-right? [[x y]]
  (and (< 30 x) (< 30 y))
  )






;; determines which quarter of the arena the tron is in

(defn find-quadrant [pos]
  (cond
   (top-left? pos) :top-left
   (top-right? pos) :top-right
   (bottom-left? pos) :bottom-left
   (bottom-right? pos) :bottom-right
   )

  )




;; gives the position of the surrounding coordinates
(defn surrounding-coordinates [[x y]]

      [(up[x y]) (down[x y]) (left[x y]) (right[x y])]

  )


;; determines which moves are valid
(defn valid-moves [look [x y]]

  (filter (fn [z] (nil? (look z))) (surrounding-coordinates [x y]))


)


(defn priority-move [look priorities pos]
  (first (map nil? (map look (map (fn [p] (p pos)) priorities))))
  )


(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos (first(valid-moves look[x y]))})





;; launch bots











