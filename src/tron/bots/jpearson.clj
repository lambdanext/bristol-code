(ns tron.bots.jpearson
  (:require [tron.core :as tron]))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn add-vectors-destructured [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn neighbours [currentposvector offsets]
  (map (partial add-vectors-destructured currentposvector) offsets))

(defn validneighbours [look pos]
  (remove look (neighbours pos directions)))

(defn rastermouse
  [look {pos :pos}]
    {:pos (first (validneighbours look pos))}
  )



