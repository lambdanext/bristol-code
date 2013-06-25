(ns tron.bots.joprem
  (:require [tron.core :as tron]))

;; call mybuzz  


(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn addv [v1 v2]
  (map + v1 v2))

(defn direction [cur dir]
  (addv dir cur))

(defn filter-bad-moves [lok pos]
   (nil? (lok pos)))

(defn valid-moves [lok pos]
  (filter (partial filter-bad-moves lok) pos))

(defn possible-moves [pos]
 (map (partial direction pos) directions))

(defn sq [x]
  (* x x))

(defn get-distance [[ox oy] [cx cy]]
  (let [x (- ox cx)
        y (- oy cy)]
  (+ (Math/pow x 2) (Math/pow y 2))))

(defn calculate-pos [dis ran]
  (* dis ran))

(defn choose-move [pos origin]
  (when (seq pos)
    (let [ran (rand-nth pos)
          dis (get-distance origin ran)]
     ran)))

(map (partial calculate-pos 8) [5 3])

(defn mybuzz
  "buzz"
  [look state]
  (let [x (first (:pos state))
        y (second (:pos state))]
    {:pos (choose-move (valid-moves look (possible-moves [x y])) [x y])}))

(defn down-or-right
    [look {pos :pos}]
    (if (look (down pos))
      {:pos (right pos)}
      {:pos (down pos)}))

(defn up-or-left
    [look {pos :pos}]
    (if (look (up pos))
      {:pos (left pos)}
      {:pos (up pos)}))

