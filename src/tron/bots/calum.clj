(ns tron.bots.calum
  (:require [tron.core :as tron]))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))


(def directions
  [[-1 0] [1 0] [0 -1] [0 1]])

(defn addv [v1 v2]
  (map + v1 v2))

(defn is-blocked? [look move]
  (look move))

(defn my-looks [look possible-moves]
  (remove (fn [p] (is-blocked? look p)) possible-moves))

(defn valid-moves-2 [pos]
  (map (fn [p] (addv pos p)) directions))

(defn sq [x] (* x x))

(defn distance [[ax ay] [bx by]]
  (let [dist (+ (sq (- ax bx)) (sq (- ay by)))
        _ (prn "Distance between " ax ay " and " bx by " is " dist)]
    dist))

(defn best-possibility [look start current-pos possibilities]
  (apply min-key (fn [x] (distance start x)) possibilities))

(defn get-smarter-new-pos [look start current-pos]
  (let [possibilities (my-looks look (valid-moves-2 current-pos))]
    (best-possibility look start current-pos possibilities)))

(defn newbuzz [look {pos :pos start :start}]
  (let [start (or start pos) ]
    {:pos (get-smarter-new-pos look start pos)
     :start start})
  )

