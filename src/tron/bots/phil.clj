(ns tron.bots.phil
  (:require [tron.core :as tron]))

(defn move [[x y][dx dy]]
  [(+ x dx) (+ y dy)])

(defn neighbours [pos]
  (map #(move pos %) tron/legal-moves))

(defn unnoc-neighbours [pos look]
  (filter #(nil? (look %)) (neighbours pos)))

(defn square 
  "Convenience method to calculate the square of a number"
  [numb] 
  (* numb numb))

(defn distance 
 "Calculates the Euclidean distance between two cells"
  [{[x y] :pos} [old-x old-y]]
 [[old-x old-y] (Math/sqrt(+ (square (- old-x x)) (square (- old-y y))))])

(defn min-to-orig 
  "Returns the available cell closest to the origin"
  [{pos :pos} origin look]
  (ffirst (sort-by second (map #(distance origin %) (unnoc-neighbours pos look)))))


(defn max-to-orig 
 "Returns the available cell most distant from the origin" 
  [{pos :pos} origin look]
  (first (last (sort-by second (map #(distance origin %) (unnoc-neighbours pos look))))))

;; My "buzz" function.....
(defn home-b 
  "Tries to remain as close to home as possible"
  [look state-map] 
  (let [origin (get state-map :origin state-map)] 
    {:pos (min-to-orig state-map origin look) :origin origin}))    
