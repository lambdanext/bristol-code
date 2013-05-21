(ns tron.bots.paddy
  (:require [tron.core :as tron]))

;; Start of max-moves-strategy implementation

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn count-moves [moves look] (count (take-while (complement look) moves)))
;; (count-moves (iterate right [1 0]) mock-look)
;; (count-moves (iterate left [5 5]) mock-look)
;; (count-moves (iterate up [0 0]) mock-look)
;; (count-moves (iterate down [0 4]) mock-look)

(defn count-moves-right [pos look] (count-moves (rest (iterate right pos)) look))
;; (count-moves-right [1 0] mock-look)
;; (count-moves-right [2 1] mock-look)

(defn count-moves-left [pos look] (count-moves (rest (iterate left pos)) look))
;; (count-moves-left [8 10] mock-look)

(defn count-moves-up [pos look] (count-moves (rest (iterate up pos)) look))
;; (count-moves-up [0 9] mock-look)

(defn count-moves-down [pos look] (count-moves (rest (iterate down pos)) look))
;; (count-moves-down [10 1] mock-look)


(defn max-valid-moves [pos look] 
  (max (count-moves-up pos look)
       (count-moves-down pos look)
       (count-moves-left pos look)
       (count-moves-right pos look)))
;; (max-valid-moves [5 3] mock-look)



(defn select-max-move [pos look]
  (let [max-moves (max-valid-moves pos look)
        up-moves (count-moves-up pos look)
        down-moves (count-moves-down pos look)
        left-moves (count-moves-left pos look)
        right-moves (count-moves-right pos look)]
    (cond 
      (= up-moves max-moves) (up pos)
      (= down-moves max-moves) (down pos)
      (= left-moves max-moves) (left pos)
      (= right-moves max-moves) (right pos))))

;(select-max-move [2 8] mock-look)


(defn max-moves-strategy
  [look {pos :pos}]
  {:pos (select-max-move pos look)})