(ns tron.bots.braindead
  (:require [tron.core :as tron]))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn move
  [[x y] [dx dy]]
  [(+ (or x 0) dx) (+ (or y 0) dy)])

(defn dirs
  [pos directions]
  (map move (repeat (count directions) pos) directions))

(defn simple-pos
  [look pos dir prevpos directions]
  (let [dir (if (= dir [nil nil])
              [0 0]
              dir)
        ps1 (dirs pos directions)
        ps2 (remove #(= prevpos %) ps1)
        npos (first (remove look ps2))]
    {:pos npos
     :dir [(- (npos 0) (pos 0)) (- (npos 1) (pos 1))]
     :prevpos pos}))

;; simple-buzz is entry point
(defn simple-buzz
  [look {[x y] :pos [dx dy] :dir [px py] :prevpos}]
  (let [ret (simple-pos look [x y] [dx dy] [px py] directions)]
    ret))
