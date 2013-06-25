(ns tron.bots.matthew
  (:require [tron.core :as tron]))

(defn buzz
  "To infinity and beyond!"
  [look {[x y] :pos}]
  {:pos [(inc x) y]})

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn down [[x y]]
  [x (inc y)])

(defn down-or-right
  [look {pos :pos}]
  (if (look (down pos))
    {:pos (right pos)}
    {:pos (down pos)}))

(defn next-coords
  [coord]
  [(up coord) (down coord) (left coord) (right coord)])

(defn dummy-look
 [coord]
 (if (< (rand) 0.5)
   :wall
   nil))

(defn go-mad
  []
  (> (rand) 0.8))

(for [x (range 10) y (range 1)] [x y])

(defn left-locations
 [coord]
 (let [x-vals (range 0 (dec (first coord)))
       y-vals (range 0 60)
       coords (for [x x-vals y y-vals] [x y])]
   coords)
 )

(left-locations [2 0])

(defn right-locations
  [coord]
  (let [x-vals (range (inc (first coord)) 59)
        y-vals (range 0 60)
        coords (for [ x x-vals y y-vals] [x y])]
    coords))

(defn up-locations
  [coord]
  (let [x-vals (range 0 60)
        y-vals (range 0 (dec (second coord)))
        coords (for [x x-vals y y-vals] [x y])]
    coords))

(defn down-locations
  [coord]
  (let [x-vals (range 0 60)
        y-vals (range (inc (second coord)) 59)
        coords (for [x x-vals y y-vals] [x y])]))

(defn density
  [look locfunc coord]
  (let [coords (locfunc coord)
        num (count coords)
        used (filter identity (map look coords))
        used-count (count used)]
    (- num used-count)))

(down-locations [10 10])

(density dummy-look up-locations [10 59])

(defn valid-next-coords
  [look coord]
  (remove look (next-coords coord)))

(defn mybuzz
  "To infinity and beyond!"
  [look state]
  (let [previous (:previous state)
        [cx cy :as curr] (:pos state)
        valid-next (valid-next-coords look curr)]
    (if (nil? previous)
      {:pos (rand-nth valid-next) :previous (:pos state)}
      (let [[px py :as prev] previous
            same-dir [(- (* 2 cx) px) (- (* 2 cy) py)]]
        (if (or (look same-dir) (go-mad))
          {:pos (rand-nth valid-next) :previous (:pos state)}
          {:pos same-dir :previous curr}))))
  )

;; (defn mybuzz
;;   "To infinity and beyond!"
;;   [look state]
;;   (let [previous (:previous state)
;;         [cx cy :as curr] (:pos state)
;;         valid-next (valid-next-coords look curr)]
;;     (if (nil? previous)
;;       {:pos (rand-nth valid-next) :previous (:pos state)}
;;       (let [[px py :as prev] previous
;;             same-dir [(- (* 2 cx) px) (- (* 2 cy) py)]
;;             left (density look left-locations curr)
;;             right (density look right-locations curr)
;;             up (density look up-locations curr)
;;             down (density look down-locations curr)
;;             max (max left right up down)]
;;         (if (and (= left max) (not (look left)))
;;           {:pos (left)})
;;         (if (or (look same-dir) (go-mad))
;;           {:pos (rand-nth valid-next) :previous (:pos state)}
;;           {:pos same-dir :previous curr}))))
;;   )

;; launch bots

