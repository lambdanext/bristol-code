(ns tron.core
  (:require [quil.core :as q]))

(def size "size of the square arena" 30)
(def scale 20)
(def sleep-length "time in ms between turns" 200)

(def arena
  (vec
    (map vec (partition size
               (repeatedly (* size size) #(ref nil))))))

(defn setup []
  (q/color-mode :hsb)
  (q/smooth)
  (q/frame-rate 10))

(defn draw []
  (q/background 0)
  (dosync 
    (doseq [x (range 0 size)
            y (range 0 size)]
      (when-let [hue @(get-in arena [x y])]
        (q/fill (q/color hue 255 255))
        (q/rect (* scale x) (* scale y) scale scale)))))

(q/defsketch tron
  :title "TRON"
  :setup setup
  :draw draw
  :size [(* scale size) (* scale size)])

(defn valid-pos? [i j]
  (and (< -1 i size) (< -1 j size)))

(def dirs {:down [0 1] 
           :right [1 0] 
           :up [0 -1]
           :left [-1 0]})

(defn next-pos [i j dir]
  (let [[di dj] (dirs dir)
        i (+ i di)
        j (+ j dj)]
    [i j]))

(defn biker [strategy]
  (fn self [{hue :hue [i j] :pos :as state}]
    (let [dir (strategy i j)
          pos (when dir (next-pos i j dir))
          cell (when pos (get-in arena pos))
          moved (dosync 
                  (when (and cell (nil? @cell))
                    (ref-set cell hue)
                    :ok))]
      (if moved
        (do
          (Thread/sleep sleep-length)
          (send-off *agent* self)
          {:hue hue :pos pos})
        (do 
          (println "arghhh" hue)
          (assoc state :dead true))))))

(defn spawn-biker [strategy]
  (send-off (agent {:pos [(rand-int size)
                          (rand-int size)]
                    :hue (rand-int 255)})
        (biker strategy)))

#_(spawn-biker (constantly :right))

; ideas:
; * less stupid bots
; * wall disappearing when a bot dies
; * faster when closer to a wall
; * visibility cone/range
; * limiting the cheating capacity of 
;   a strategy







