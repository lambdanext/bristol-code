(ns tron.bots.katherine
  (:require [tron.core :as tron]))

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn next-moves [pos]
  [(up pos) (down pos) (left pos) (right pos)])


(defn num-neigh [look pos]
  [pos (count (remove look (next-moves pos)))]
  )

(defn valid-next-moves [look pos]
  (let [moves (remove look (next-moves pos))]
    (take 2(reverse(sort-by second (map (partial num-neigh look) moves))))))

(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  (let [next-moves (valid-next-moves look [x y])]
    {:pos (when (seq next-moves) (first (rand-nth next-moves)))}))


(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

;; launch bots

