(ns tron.bots.dansmithy
  (:require [tron.core :as tron]))

;; INCLUDE THE NAME OF THE BOTT FN AT THE TOP

;; Bot = danny

(defn empty-look
  "A mock look function which just checks for the arena
   boundaries."
  [pos]
  (when-not (tron/valid-pos? pos) :wall))

(defn mock-look
  "Returns a mock look function which checks for the arena
   boundaries and the specified occupied positions."
  [& occupied-poses]
  (let [occupied-poses (set occupied-poses)]
    (fn [pos]
      (or (occupied-poses pos)
        (when-not (tron/valid-pos? pos) :wall)))))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})


(defn left [[x y]]
  [(dec x) y])

(defn up [[x y]]
  [x (dec y)])

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn adddelta
  [delta pos]
  (vec (map + pos delta)))

(def directions
  {:left [-1 0]
   :right [1 0]
   :up [0 -1]
   :down [0 1]})

(defn neighbours
  [pos]
  (for [delta (vals directions)]
    (adddelta delta)))

(def turns
  [left down right up])

(def clockwise [up right down left])
(def anticlockwise (vec (reverse clockwise)))

(defn rot-map
  [rotation]
  (zipmap
   (cons (peek rotation) rotation)
   rotation))
(def rot-cw (rot-map clockwise))
(def rot-acw (rot-map anticlockwise))


(defn valid-neighbours
  [look pos]
  (remove look (neighbours pos)))


(defn keep-going
  [look direction pos]
  (take-while (complement look) (rest (iterate direction pos))))

(defn firstdir
  [pos]
  right)

(defn valid-turn2
  [look pos dir rotationmap]
  (first (remove #(look (% pos)) (take 4 (iterate rotationmap dir)))))

(defn valid-turn
  [look pos dir]
  (first (remove #(look (% pos)) (take 4 (iterate rot-cw dir)))))

(defn new-dir
  [look pos dir]
  (if (look (dir pos))
    (rot-cw dir)
    dir))

(defn circles
  "To infinity and beyond!"
  [look m]
  (let [pos (:pos m)
        dir (get m :lastdir (firstdir pos))
        dir (valid-turn look pos dir)]
    {:pos (dir pos)
     :lastdir dir}))

(defn new-dir-out
  [look pos dir rotationmap]
  (let [poss-newdir (rotationmap dir)]
    (if (look (poss-newdir pos))
      (valid-turn2 look pos dir rotationmap)
      poss-newdir)))

(defn danny
  "To infinity and beyond!"
  [look m]
  (let [pos (:pos m)
        rotationmap (get m :rotationmap rot-acw)
        dir (:lastdir m (firstdir pos))
        dir (new-dir-out look pos dir rotationmap)
        dir (if dir dir right)
        ]
    {:pos (dir pos)
     :lastdir dir
     :rotationmap rotationmap}))

;; launch bots

;; Should do tight loops. But then stops at some point and starts doing big loops. Didn't get that fixed.
