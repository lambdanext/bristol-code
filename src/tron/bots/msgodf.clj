(ns tron.bots.msgodf
  (:require [tron.core :as tron]))

(defn empty-look
  "A mock look function which just checks for the arena
   boundaries."
  [pos]
  (when-not (tron/valid-pos? pos) :wall))

(defn mock-look
  "Returns a mock look function which checks for the arena
   boundaries and the specified occupied positions."
  [& occupied-poses]
  (let [occupied-poses (set occupied-poses)]
    (fn [pos]
      (or (occupied-poses pos)
        (when-not (tron/valid-pos? pos) :wall)))))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn neighbours
  "Returns a vector of the four neighbours of the supplied position"
  [pos]
  (map #(% pos) [up down left right]))

(defn valid-neighbours
  [look pos]
  (filter #(not (look %))
          (neighbours pos)))

(defn invalid-neighbours
  [look pos]
  (filter look
          (neighbours pos)))

(defn valid-moves
  [look pos]
  (filter #(not (look (% pos)))
          [up down left right]))

(defn happy-moves
  [look pos]
  (filter #(not (look (% pos)))
          [up down left right]))

(defn positions-to-right
  [look pos]
  (take-while #(not (look %))
              (iterate right pos)))

(defn positions-to-right-using-for
  "This gets all the positions to the right using for"
  [look pos]
  (for [p (iterate right pos) :while #(not (look p))] p))

(positions-to-right-using-for empty-look [10 10])

(defn positions-in-direction
  [look pos direction]
  (take-while #(not (look %))
              (iterate direction pos)))

(positions-in-direction empty-look [10 10] up)

;; If this one sees something above it then it goes right
(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

;; If this one sees something below it then it goes left
(defn up-or-left
  [look {pos :pos}]
  (if (look (up pos))
    {:pos (left pos)}
    {:pos (up pos)}))

(defn random-track-everyone
  "Keeps track of all previous positions, plus things it has seen!"
  [look {pos :pos
         last-positions :last-positions
         :or {last-positions #{}}}]
  {:pos (let [move (remove #(contains? last-positions %)
                           (valid-neighbours empty-look pos))]
          (if (empty? move) pos (rand-nth move)))
   :last-positions (into (conj last-positions pos)
                         (invalid-neighbours empty-look pos))})

(defn all-valid-moves
  [look pos history]
  (remove #(contains? history (% pos))
          (valid-moves look pos)))

(defn msgodf-random-indecisive
  [look {pos :pos
         last-positions :last-positions
         moves :moves
         :or {last-positions #{}
              moves []}}]
  (if (empty? moves)
    (let [moves (all-valid-moves empty-look pos last-positions)
          move (if (empty? moves) nil (rand-nth moves))]
      {:pos (if move (move pos) pos)
       :last-positions (into (conj last-positions pos) (invalid-neighbours empty-look pos))
       :moves (repeat (+ 5 (rand-int 4)) move)})
    (if (look ((apply comp moves) pos))
      (let [moves (all-valid-moves empty-look pos last-positions)
            move (if (empty? moves) nil (rand-nth moves))]
        {:pos (if move (move pos) pos)
         :last-positions (into (conj last-positions pos) (invalid-neighbours empty-look pos))
         :moves (repeat (+ 5 (rand-int 4)) move)})
      {:pos ((first moves) pos)
       :last-positions (into (conj last-positions pos) (invalid-neighbours empty-look pos))
       :moves (rest moves)})))

;; launch bots
