(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(def grid-size 10)
(def cell-size 10)

(defn pos->xy [world pos]
  [(mod pos (Math/sqrt (count world))) ; x = position % world size
   (quot pos (Math/sqrt (count world))) ; y = round down to lowest multiple of world size 
   ])

(defn xy->pos [world x y]
  (let [max-pos (count world)
        grid-size (Math/sqrt max-pos)]
    (int (+ (* grid-size y) x))))

(defn neighbour-pos [world pos]
  (let [[x y] (pos->xy world pos)
        max-coord (count world)
        coords (for [nx (range (dec x) (+ 2 x))
                     ny (range (dec y) (+ 2 y))
                     :when (and (not= [x y] [nx ny])
                                (<= 0 nx)
                                (<= 0 ny)
                                (< nx max-coord)
                                (< ny max-coord))]
                 [(int nx) (int ny)])]
    (mapv #(apply (partial xy->pos world) %) coords)))

(defn evaluate-cell-life [world pos]
  (let [neighbour-pos (neighbour-pos world pos)
        neigh-vals (mapv (into [] world) (filter #(< % (count world)) neighbour-pos))
        total-val (apply + neigh-vals)
        alive? (= (nth world pos) 1)]
    (cond
      (and alive? (< 1 total-val 4)) 1
      (= 3 total-val) 1
      :else 0)))

(defn life-setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (q/no-stroke)
  {:world (into [] (take (* grid-size grid-size) (repeatedly #(rand-int 2))))})

(defn update-life [{world :world}]
  {:world (mapv #(evaluate-cell-life world %) (range (count world)))})

(defn draw-life [{world :world}]
  ; clear canvas
  (q/background 240)
  (q/fill 255 255 255)
  (doseq [pos (range (count world))]
    (let [[x y] (pos->xy world pos)
          cell-value (nth world pos)]
      (q/fill (if (= 0 cell-value) 255 0))
      (q/rect (* x cell-size) (* cell-size y) cell-size cell-size))))

(defn -main [& _]
  (println "starting animation")
  (q/defsketch life
    :title "game of life"
    :size [1000 1000]
    :setup life-setup
    :update update-life
    :draw draw-life
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

(time (take 1 (iterate update-life {:world (into [] (take (* grid-size grid-size) (repeatedly #(rand-int 2))))})))

; took ~9000 msec
(time (dotimes [i 10000]
        (update-life {:world (into [] (take (* 10 10) (repeatedly #(rand-int 2))))})))



