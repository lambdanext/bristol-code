(ns tron.bots.sirob
  (:require [tron.core :as tron]))




(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])


(defn up-or-left
  [look {pos :pos}]
  (if (look (up pos))
    {:pos (left pos)}
    {:pos (up pos)}))


(defn possible-coords [x y]
  [(up [x y]) (left [x y]) (down [x y]) (right [x y])]
)


(defn allowed-moves [look potential-positions]
  (filter #(nil? (look %)) potential-positions)
)



  
(defn wall-near [look [x y]]
  
  (cond
     (= (look [x (- y 2)]) :wall) :up
     (= (look [x (+ y 2)]) :wall) :down
     (= (look [(- x 2) y]) :wall) :left
     (= (look [(+ x 2) y]) :wall) :right
    :else nil
   )
)


(defn wall-strategy

  [look {[x y] :pos}]

    (cond

     
     (and (not (look (up [x y])))
       (not(= (wall-near look [x y]) :up) ))
          {:pos (up [x y])}
     
     (and (not (look (left [x y])))
       (not(= (wall-near look [x y]) :left) ))
          {:pos (left [x y])}

     (and (not (look (down [x y])))
       (not(= (wall-near look [x y]) :down) ))
          {:pos (down [x y])}

     (and (not (look (right [x y])))
       (not(= (wall-near look [x y]) :right) ))
          {:pos (right [x y])}


    :else 
        (if (look (down [x y]))
          {:pos (right [x y])}
          {:pos (down [x y])})
    )
    
    
  )
  








