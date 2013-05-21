(ns tron.bots.neil
  (:require [tron.core :as tron]))

(def directions {:right [1 0]
                 :down  [0 1]
                 :left  [-1 0]
                 :up    [0 -1]})
 
(def pos [3 4])
 
(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])
 
(defn up [pos]
  (move pos (:up directions)))
 
(defn down [pos]
  (move pos (:down directions)))
 
(defn left [pos]
  (move pos (:left directions)))
 
(defn right [pos]
  (move pos (:right directions)))
 
(defn possible-moves [pos] (map move (repeat pos) (vals directions)))
 
(defn buzz
  "To the infinity and beyond!"
  [look state]
  (let [pos (:pos state )]
    {:pos (right pos)}))
 
(defn all-above [pos]
  (let [coords (iterate up pos)]
    (take-while tron/valid-pos? coords)))
 
(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))
 
(defn circle
  [look {pos :pos}]
  (let [ns (possible-moves pos)
        ns (remove look ns)]
    {:pos (first ns)}))
 
(defn live-space
  [look x y grid]
  (let [r (look [x y])]
    (cond
     (> x tron/size) [grid]
     (> y tron/size) [grid]
     (= [x y] [tron/size tron/size]) [grid]
     (= r :wall) (recur look 1 (inc y) (conj grid (set [x y 0])))
     (nil? r) (recur look (inc x) y (conj grid (set [x y -1])))
     :else (recur look (inc x) y (conj grid (set [x y r]))
            )))
  )
 
#_(defn count-right
  [grid x y acc]
  (cond
       (= ) ))
 
  ;; launch two buzzes
