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
    (let [new-state (strategy state)
          pos (when new-state 
                (next-pos i j (:dir new-state)))
          cell (when pos (get-in arena pos))
          moved (dosync 
                  (when (and cell (nil? @cell))
                    (ref-set cell hue)
                    :ok))]
      (if moved
        (do
          (Thread/sleep sleep-length)
          (send-off *agent* self)
          (assoc new-state :hue hue :pos pos))
        (do 
          (println "arghhh" hue)
          (assoc state :dead true))))))

(defn spawn-biker [strategy]
  (send-off (agent {:pos [(rand-int size)
                          (rand-int size)]
                    :hue (rand-int 255)})
        (biker strategy)))

#_(spawn-biker (constantly :right))

(def kamikaze (constantly {:dir :right}))

(defn stubborn [{[i j] :pos}]
  (let [pos (next-pos i j :right)
        cell (get-in arena pos)]
    (if (and cell (nil? @cell))
      {:dir :right}
      {:dir :up})))

; ideas:
; * less stupid bots
; * wall disappearing when a bot dies
; * faster when closer to a wall
; * visibility cone/range
; * limiting the cheating capacity of 
;   a strategy

(defn adapter 
  "Creates a v2 strategy from a v1 strategy"
  [old-strategy]
  (fn [state]
    (when-let [dir (apply old-strategy (:pos state))]
      {:dir dir}) ))

;;;;;;;;;;;;

(defn ferrari [{:keys [dir togo] :or {togo 0}}]
  (if (pos? togo)
    {:dir dir :togo (dec togo)}
    {:dir (rand-nth [:up :down :left :right])
     :togo 5}))

;;;;;;;;;;;


(defn free? 
   [pos]
   (nil? (deref (get-in arena pos)))
)


(defn free-distance
  [pos dir]
  (count
   (take-while
     (fn [[i j ]] ( and (valid-pos? i j) (free?  [ i j])))
     (rest
       (iterate (fn [[ i j]] (next-pos i j dir)) pos)
     )
     )
   )
  )
(defn better-direction
  [pos dir1 dir2]
  (if (> (free-distance pos dir1) (free-distance pos dir2))
    dir1
    dir2
    )
  )

(defn best-direction
  [pos]
  (println pos)
  (reduce (fn [dir1 dir2] (better-direction pos dir1 dir2)) (keys dirs))
  )

(defn smartass [{pos :pos}]
  "strategy to to dodge walls and other racers"
  {:dir (best-direction pos)}
  )

;;;;;;;;;;;;


(def directions [:up :down :left :right])

(defn possible-dirs [{[ i j] :pos}]
  (->> directions
       (map (partial next-pos i j))
       (map (partial get-in arena))
       (zipmap directions)
       (filter #(and (val %) (nil? (deref (val %)))) )))

(defn random-dir [state]
  (let [dirs (possible-dirs state)]
    (rand-nth (vec dirs))))

(defn random-strategy [state]
  {:dir ((random-dir state) 0)})


(defn raster-dir [state]
  (let [dirs (possible-dirs state)]
    (first (vec dirs))))

(defn raster-strategy [state]
  {:dir ((raster-dir state) 0)})

;;;; Launch them all!!

#_(doseq [s [ferrari smartass 
             random-strategy raster-strategy]]
    (spawn-biker s))

