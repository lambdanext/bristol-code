(ns tron.bots.gav
  (:require [tron.core :as tron]))

; (in-ns 'tron.bots)
; or
; (require :reload-all 'tron.core)
(def directions #{[1 0] [0 1] [-1 0] [0 -1]})
(def directions [[1 0] [0 1] [-1 0] [0 -1]])


(defn move [[x y] [dx dy]] [(+ x dx) (+ y dy)])

;(map move (repeat my-pos) directions)
; binary vs unary map function
(defn neighs [p]
  (let [all-neighs (map #(move p %) directions)
        valid-neighs (filter tron/valid-pos? all-neighs)]
    valid-neighs))

(defn against-wall? [look {[x y] :pos}]
  (let [nbs (neighs [x y])
        good-nbs (map look nbs)]
    (<= (count (filter nil? good-nbs)) 2)))

;MAIN FUNCTION
(defn better-buzz
  "To the infinity and beyond!"
  [look {[x y] :pos [dx dy] :dir}]
  (if (nil? dx)
    (better-buzz look {:pos [x y] :dir (first directions)})
    (if (against-wall? look {:pos [x y]})
      (let [[nx ny] (first (neighs [x y]))
            ndx (- nx x)
            ndy (- ny y)
            ]
        {:pos [nx ny]  :dir [ndx ndy]}
        )
      {:pos (move [x y] [dx dy]) :dir [dx dy]}
      )
    )
  )


(defn up [p] [(p 0) (inc (p 1))])

;(defn all-above [p] (iterate up p))
(defn all-above [p] (take-while tron/valid-pos? (iterate up p)))

;all above that are valid

#_(doseq [s [better-buzz]]
  (tron/spawn-biker s));