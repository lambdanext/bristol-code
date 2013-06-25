(ns tron.bots.mt3593
  (:require [tron.core :as tron]))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})



(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x)  y])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

;; All valid next moves
(defn valid-moves [pos]
  [(right pos) (left pos) (up pos) (down pos)])

(defn next-moves [look moves]
  (filter (comp nil? look) moves))


(defn bot
  [look {pos :pos}]
  (let [nm  (first (next-moves look (valid-moves pos)))]
    (println nm)
    (println pos)
    {:pos nm}))

(defn garenteed-next-move [look pos]
  {:pos pos :count  (count (next-moves look (valid-moves pos)))})

(defn forsight-move [look pos]
  (:pos (first (reverse (sort-by :count (map #(garenteed-next-move look %) (next-moves look (valid-moves pos))))))))

(defn future-bot
  [look {pos :pos}]
  (let [nm (forsight-move look pos)]
    (println nm)
    {:pos nm}))



(defn weight [path look pos count]
  (if (or (reduce contains? path pos) (= count 0) )
    path
    (map #(weight (clojure.set/union path pos) look  #{ %} (dec count)) (next-moves look (valid-moves (first pos))))))

(defn best-weight [look pos] (first (reverse (sort (map count (weight #{} look #{pos} 100))))))

(defn best-possible-move [look pos]
  (:pos (first (reverse (sort-by :count (map (fn [x] {:pos x :count (best-weight look x)})  (next-moves look (valid-moves pos))))))))

(defn robot
  "think"
  [look {pos :pos}]
  (let [nm  {:pos (best-possible-move look pos)}]
    (println nm)
    nm))

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))


