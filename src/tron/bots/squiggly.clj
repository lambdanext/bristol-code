; Yolina
(ns tron.bots.squiggly
  (:require [tron.core :as tron]))

(def mock-bots
  {[2 0] true
    [5 3] true})

(defn mock-look
  [pos]
  (if (tron/valid-pos? pos)
    (get mock-bots pos) :wall))

(def directions #{[1 0] [0 1] [-1 0] [0 -1]})
(def poss [0 0])
; (defn move [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn right [[x y]] [(inc x) y])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(- x 1) y])
(defn up [[x y]] [x (- y 1)])

(defn all-valid [f pos] (rest (iterate f pos)))
(defn valids [look f pos] (count (take-while (complement look) (all-valid f pos))))

; Call this function
(defn move
  [look {pos :pos}]
  (let [dirs [up down left right]
      best (apply max-key #(valids look % pos) dirs)]
    {:pos (best pos)})
  )
