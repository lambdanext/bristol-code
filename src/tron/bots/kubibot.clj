(ns tron.bots.kubibot
  (:require [tron.core :as tron]))

; (in-ns 'tron.bots)
; or
; (require :reload-all 'tron.core)
; (use 'clojure.repl)
(def directions #{[1 0] [0 1] [-1 0] [0 -1]})
(def directions [[1 0] [0 1] [-1 0] [0 -1]])


(defn move [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn down [pos]
  (move pos [0 1]))

(defn up [pos]
  (move pos [0 -1]))

;(map move (repeat my-pos) directions)
; binary vs unary map function
(defn neighs [look p]
  (let [all-neighs (map #(move p %) directions)
        valid-neighs (remove look all-neighs)]
    valid-neighs))

(defn diff [[x y] [x2 y2]]
  [(- x2 x) (- y2 y)])

(defn fwd [[x y] [dx dy]]
  [(+ x dx) (+ y dy)]
  )


(defn up [p] [(p 0) (inc (p 1))])

;(defn all-above [p] (iterate up p))
(defn all-above [p] (take-while tron/valid-pos? (iterate up p)))

(defn kubibot
  "To the infinity and beyond!"
  [look {[x y] :pos d :dir}]
  
  (cond
    (nil? d) 
    (let [move (first (neighs look [x y]))
          dir (diff [x y] move)]
      {:pos move :dir dir})
    
    (nil? (look (fwd [x y] d))) 
    {:pos (fwd [x y] d) :dir d}
    
    :else
    (let [move (first (neighs look [x y]))
          dir (diff [x y] move)]
      {:pos move :dir dir})
    )  
)

(defn fake-look [pos]
  :wall)
























































(defn against-wall? [look {[x y] :pos}]
  (let [nbs (neighs [x y])
        good-nbs (map look nbs)]
    (<= (count (filter nil? good-nbs)) 2)))


(defn find-wall-move [look opts]
  (let [wall-moves (filter #(against-wall? look %) opts)]
    (if (> (count wall-moves) 0)
      (first wall-moves)
      (first opts))))



(defn better-buzz
  "To the infinity and beyond!"
  [look {[x y] :pos [dx dy] :dir}]
  (println "against wall:" (look (down [x y])))
  ;;(printf "something %s" (vec (find-wall-move look (remove look (neighs [x y])))))
  (if (nil? dx)
    (better-buzz look {:pos [x y] :dir (first directions)})
    (if (against-wall? look {:pos [x y]})
      ;(let [[nx ny] (first (remove look (neighs [x y])))
      (let [[nx ny] (find-wall-move look (remove look (neighs [x y])))
            ndx (- nx x)
            ndy (- ny y)
            ]
        {:pos [nx ny]  :dir [ndx ndy]}
        )
      {:pos (move [x y] [dx dy]) :dir [dx dy]}
      )
    )
  )
