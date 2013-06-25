(ns tron.bots
  (:require [tron.core :as tron]))

(defn buzz 
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

;; launch bots
(doseq [[s hue sym] 
        (map (fn [sym hue]
               [@(resolve sym) hue sym]) 
            '[tron.bots.calum/newbuzz
             tron.bots.mary/buzz
             tron.bots.matthew/mybuzz
             tron.bots.mt3593/robot
             tron.bots.alexander/harold
             tron.bots.joprem/mybuzz
             tron.bots.katherine/buzz
             tron.bots.james/spiral-buzz
             tron.bots.jpearson/rastermouse
             tron.bots.benjamin/clockwise
             tron.bots.hector/buzz
             tron.bots.sirob/wall-strategy]
            (iterate #(+ % 25) 0))]
  (tron/spawn-biker s hue sym))