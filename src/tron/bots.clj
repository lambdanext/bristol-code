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
(doseq [[s hue] 
        (map vector 
            [tron.bots.ahoy/turn-based-strat
             tron.bots.gav/better-buzz
             tron.bots.neil/circle
             tron.bots.xianralph/switcheroo
             tron.bots.braindead/simple-buzz
             tron.bots.kubibot/kubibot
             tron.bots.phil/home-b
             tron.bots.paddy/max-moves-strategy
             tron.bots.squiggly/move]
            (iterate #(+ % 25) 0))]
  (tron/spawn-biker s hue))