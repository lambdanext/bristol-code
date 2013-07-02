(ns tron.bots.nicolas-ginder
  (:require [tron.core :as tron]))

(defn right [[x y]]
  [(inc x) y])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn up [[x y]]
  [x (dec y)])


(defn neighbours [pos]

  [(up pos) (right pos) (down pos) (left pos)]
  )

(defn valid-neighbours [look pos]
  (remove look (neighbours pos))
  )


(defn crazy-one
  [look {pos :pos last :lastpos :as m}]

  (let [newpos (rand-nth (valid-neighbours look pos))]

    (if (= newpos last)
      (crazy-one look m)
      {:pos  newpos
       :lastpos last}))
 )

(defn random-valid-direction [look {pos :pos}]

  (let [direction (rand-nth  (remove look [(right pos) (left pos) (up pos) (down pos)]))]
    {:pos direction
     })

  )

(defn clever-one
  [look {pos :pos last :last :as m :or {last right}}]


  (if (look (last pos))

    {:pos (:pos (crazy-one look m))
     :last (rand-nth  [right left up down])}
    {:pos (last pos)
     :last last}

    )


 )



