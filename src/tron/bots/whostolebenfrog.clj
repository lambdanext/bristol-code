(ns tron.bots.whostolebenfrog
  (:require [tron.core :as tron]))

;; the backup bot where my real one failed
;; function name is ben


(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn neighbours [pos]
  ((juxt up down left right) pos))

(defn valid-neighbours [look pos]
  {:post [(not-any? look %)]}
  (remove look (neighbours pos)))

(defn ben
  [look {pos :pos}]
  (if (look (up pos))
    {:pos (first (valid-neighbours look pos))}
    {:pos (up pos)}))