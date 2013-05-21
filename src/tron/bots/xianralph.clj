(ns tron.bots.xianralph
  (:require [tron.core :as tron]))

(defn move [[x y] [dx dy]] [(+ x dx) (+ y dy)])
(defn up [point] (move point [0 1]))
(defn down [point] (move point [0 -1]))
(defn right [point] (move point [1 0]))
(defn left [point] (move point [-1 0]))
(def directions #{up down right left})

(defn neighbours [point]
  (map (comp next #(iterate % point)) directions))

(defn valid-neighbours [look pos]
  (filter (comp nil? look first) (neighbours pos)))

(defn cost [look candidates]
  (count (take-while (comp nil? look) candidates)))

(defn wall-costed
  [look pos]
  (let [candidates (valid-neighbours look pos)]
    (sort-by #(cost look %) candidates)))

(defn survivor [look {:keys [pos]}]
  {:pos (-> (wall-costed look pos) reverse ffirst)})

(defn most-blanks [look pos]
  (count 
   (map first (filter (comp nil? look first) (neighbours pos)))))

(defn wide-open-spaces
  [look {:keys [pos]}]
  (let [candidates (map first (filter (comp nil? look first) (neighbours pos)))]
    {:pos (last (sort-by #(most-blanks look %) candidates))}))

(defn switcheroo
  ; THIS IS THE MAIN BOT ENTRY POINT
  [look {:keys [pos steps] :as context}]
  (let [steps (or steps 0)
        result (if (< (mod steps 50) 3)
                (wide-open-spaces look context)
                (survivor look context))]
  (merge result {:steps (inc steps)})))