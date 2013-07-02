(ns tron.bots.neilprosser
  (:require [tron.core :as tron]))

;; random-fritz



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

(defn neighbours
  "My immediate neighbours"
  [[x y]]
  [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]])

(neighbours [0 0])

(defn valid-neighbours
  "My valid neighbours"
  [look pos]
  (remove look (neighbours pos)))

(valid-neighbours empty-look [0 0])
(valid-neighbours empty-look [30 30])
(valid-neighbours empty-look [59 60])

(defn valid-right
  [look [x y]]
  (take-while #(not (look %)) (map #(vector % y) (next (range x 60)))))

(defn valid-moves
  [look pos move]
  (take-while #(not (look %)) (next (iterate move pos))))

(defn up [[x y]]
  [x (dec y)])

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn random-factor
  [look pos]
  (let [new-positions (valid-neighbours look pos)]
    (rand-nth new-positions)))

(random-factor empty-look [1 2])

(defn plot-a-course
  [look pos]
  (take 100 (filter #(not (= % pos)) (next (iterate (partial random-factor look) pos)))))

(plot-a-course empty-look [1 2])

(defn certain-death?
  [look pos]
  (let [n (valid-neighbours look pos)]
    (= 0 (count n))))

(certain-death? empty-look [0 1])

(defn random-fritz
  [look {pos :pos}]
  {:pos (random-factor look pos)})

(first (plot-a-course empty-look [1 2]))
(rest (plot-a-course empty-look [1 2]))

(filter true? (map #(certain-death? empty-look %) (rest (plot-a-course empty-look [1 2]))))

(defn leads-to-certain-death?
  [look course]
  (let [death (filter true? (map #(certain-death? look %) course))]
    (not (= 0 (count death)))))



(defn fritz
  [look {pos :pos course :course}]
  (if (or
       (not course)
       (leads-to-certain-death? look course))
     (let [c (plot-a-course look pos)]
      (fritz look {:pos pos :course c}))
     {:pos (first course) :course (next course)}))



;; launch bo