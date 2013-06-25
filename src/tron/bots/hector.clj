(ns tron.bots.hector
  (:require [tron.core :as tron]))

(defn right [[x y]]
  {2 [(inc x) y]})

(defn left [[x y]]
  {4 [(dec x) y]})

(defn down [[x y]]
  {3 [x (inc y)]})

(defn up [[x y]]
  {1 [x (dec y)]})

(def all-moves [up right down left])

(defn get-surrounding-squares
  [x y]
  (into {} (map #(% [x y]) all-moves)))

(comment  (get-surrounding-squares 1 1))

(defn get-possible-moves
  [look x y]
  (let [surrounding-squares (get-surrounding-squares x y)]
    (into {} (filter (comp nil? look second) surrounding-squares))))

(comment (get (get-possible-moves (fn [x] nil) 1 1) 2))

(defn get-distance-for-direction
  [look current-pos direction]
  (loop [pos current-pos
         distance 0]
    (if (or
         (< (first pos) 0)
         (> (first pos) 40)
         (< (second pos) 0)
         (> (second pos) 40)
         (seq (look pos)))
      {:dir direction :distance distance}
      (recur (flatten (vals ((get all-moves direction) pos))) (inc distance)))))
(comment (flatten (vals ((get all-moves 1) [1 1]))))
(comment (get-distance-for-direction (fn [x] nil) [1 1] 1))


(defn get-distance-for-directions
  [look current-pos possible-moves]
  (map #(get-distance-for-direction look current-pos (first %)) possible-moves))

(comment (get-distance-for-directions (fn [x] nil) [1 1] {0 [2 2]}))

(defn find-best-direction
  [directions-with-distance]
  (apply max-key :distance directions-with-distance))

(comment (find-best-direction [{:dir 1 :distance 5}{:dir 2 :distance 20}]))

(defn get-next-direction
  [look current-pos last-dir possible-moves]
  (let [directions-with-distance (get-distance-for-directions look current-pos possible-moves)
        best-direction (find-best-direction directions-with-distance)]
    (:dir best-direction)))


(defn get-next-move
  [look current-pos last-dir possible-moves]
  (if (empty? possible-moves)
    {:pos [100 100]}
    (let [next-direction (get-next-direction look current-pos last-dir possible-moves)
          new-pos (get possible-moves next-direction)]
      {:pos new-pos :last next-direction})))

(comment (get-next-move (fn [x] nil) [1 1] 1 {1 [1 2] 2 [2 2]}))


(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos dir :last}]
  (let [possible-moves (get-possible-moves look x y)
        next-move (get-next-move look [x y] dir possible-moves)]
    next-move))


