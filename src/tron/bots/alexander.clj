(ns tron.bots.alexander
  (:require [tron.core :as tron]))

;; * harold

(def directions [ [0 -1] [-1 0] [0 1]  [1 0] ])

(defn add-vectors
  [[xa ya] [xb yb]]
  [ (+ xa xb) (+ ya yb) ]
  )

(defn sub-vectors
  [[xa ya] [xb yb]]
  [ (- xa xb) (- ya yb)])

(defn move-options
  [pos directions]
  (map (partial add-vectors pos) directions))

(defn closest-block
  [look pos direction]
  (count (take-while (fn [cords] (nil? (look cords)) (add-vectors direction pos)))))
  
(defn arrow
  [ look c-pos options ]
  (nth options (dec (apply max (map (partial closest-block look c-pos) options))))
  )

(defn luck
  [ look pos options ]
  (when
    (seq options)
    (rand-nth options))
  )

(defn harold
  "To infinity and beyond!"
  [look {c-pos :pos p-pos :p-pos}]
  {:p-pos c-pos :pos ( first (remove look (move-options c-pos directions)))
  })

