(ns tron.bots.jarek
  (:require [tron.core :as tron]))





(defn valid-neighbours [look pos]
  (remove look  pos))
  
  (defn neighbours [[x y]] 
  [[(dec x) y][x (inc y)][(inc x) y][x (dec y)]]
  )

(defn tac1 [look pos] 
  (let [valid (valid-neighbours look (neighbours pos))
        rand (rand-int (count valid))]
    (if (> (count valid) 0)
    {:pos (nth valid rand)})))

(defn jarek
  [look {pos :pos}]
   (tac1 look pos)
      
      )
  