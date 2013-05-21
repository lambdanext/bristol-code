(ns tron.bots.ahoy
(:require [tron.core :as tron]))
 
 
 
 
(defn addv [x y] (map + x y))
 
(defn diffv [x y] (map - x y))
 
(diffv [0 22] [1 2])
 
(def directions #{[1 0] [-1 0] [0 1] [0 -1]})
 
 
 
(defn neighbours "sequence des voisins d'un point"
{:arglists '([[x y]])
:ahoy "AHOY"}
[point] (->> directions
(map (partial addv point))
(filter tron/valid-pos?)))
 
 
 
 
 
(defn first-correct-bot
"To the infinity and beyond!"
[look {[x y] :pos}]
{:pos (first(filter #(= nil (look %)) (neighbours [x y])))})
 
(defn valid-neighbours [look [x y]]
(filter #(= nil (look %)) (neighbours [x y]))
)

(defn random-bot
[look {[x y] :pos}]
{:pos (rand-nth (valid-neighbours look [x y]))})
 
 

 
(defn bot-num [show map-state]
(map-state :bot-num (show (map-state :pos)))
)
 
 
 
 
(defn turn [side [x y]]
(if (= side :left)
[(- y) x]
[y (- x)]
)
)
 
 
 
(defn get-dir [pos prev-pos]
(let [res (diffv pos prev-pos)]
(if (empty? res) [1 0] (into [] res))
 
))
 
 
(defn look-around [dir]
(->> dir
(turn :right)
(iterate (partial turn :left))
(take 4)
)
)
 
(defn propose-to-follow-left [show dir pos]
 
(map (partial addv pos) (filter #(and (= nil (show (addv pos %)))
(= nil (show (addv (addv pos %) (turn :left %)))
)
)(look-around dir)))
)
 
 
 
 
(defn take-valid [posseq valid-n] (
first
(concat
(filter (set valid-n) posseq)
valid-n
 
))
)
 
(defn trace [x] (do (println x) x))
 
 
(defn weighted [show pos]
(trace (- (count (filter #(= nil (show %)) (map (partial addv pos) tron/legal-moves)
)))))
 
 
(defn true-show [pos] nil)
  
(defn weigted-valid-neighbours [show pos]
(let [valid-n (valid-neighbours show pos)]
(map :pos (sort-by :w (map #(hash-map :pos % :w (weighted show %)) valid-n)))
))
 
 
(defn left-follower-bot [show map-state]
(let [pos (map-state :pos)
dir (get-dir pos (:prev-pos map-state))
valid-n (weigted-valid-neighbours show pos)
follow-left (propose-to-follow-left show dir pos)]
(assoc map-state :pos
(take-valid follow-left valid-n ) :prev-pos pos))
)
 
 
 
 
 
(defn turn-based-strat [show map-state]
  (let [turn (map-state :turn 0)]
    (assoc (if (< turn (* 2 tron/size)) (first-correct-bot show map-state)
             (left-follower-bot show map-state)
             ) :turn (inc turn))
    )
)
 
 
