
;;amanas
;; INCLUDE THE NAME OF THE BOTT FN AT THE TOP

(ns tron.bots.amanas
  (:require [tron.core :as tron]))

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

(defn neighbours [pos]
  [(up pos)
   (down pos)
   (left pos)
   (right pos)])

(defn valid-neighbours [look pos]
  (remove look (neighbours pos)))

(defn available-positions [look dir from]
  (take-while #(not (look %)) (rest (iterate dir from))))

(defn longer-direction [look from]
  (let [functions [left right up down]
        lengths-map (zipmap (map #(count (available-positions look % from)) functions) functions)
        the-key (apply max (keys lengths-map))]
    (get lengths-map the-key)))

(defn amanas
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos ( (longer-direction look [x y]) [x y])})
