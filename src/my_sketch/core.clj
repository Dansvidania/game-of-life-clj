(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import [java.util BitSet])
  (:gen-class))

(def grid-size 100)
(def cell-size 5)

(defn pos->xy [^long world-size ^long pos]
  [(mod pos (Math/sqrt world-size)) ; x= (modulo position world-size)
   (quot pos (Math/sqrt world-size)) ; y=round down to lowest multiple of grid size 
   ])

(defn xy->pos [^long world-size [^long x ^long y]]
  (let [grid-size (Math/sqrt world-size)]
    (int (+ (* grid-size y) x))))

(defn neighbour-pos [^long world-size ^long pos]
  ;{:pre [(<= 0 pos) (<= 0 world-size) (< pos world-size)]
  ; :post [(every? #(<= 0 %) %) ; all neighbour positions are positive
  ;        (every? #(not= % pos) %) ; all neighbour positions are not equal to pos
  ;        (every? #(< % world-size) %) ; all neighbour positions are less than world size
  ;        ]}
  (let [[x y] (pos->xy world-size pos)
        coords (for [nx (range (dec x) (+ 2 x))
                     ny (range (dec y) (+ 2 y))
                     :when (and (not= [x y] [nx ny])
                                (<= 0 nx)
                                (<= 0 ny)
                                (< nx (Math/sqrt world-size))
                                (< ny (Math/sqrt world-size)))]
                 [(int nx) (int ny)])]
    (mapv (partial xy->pos world-size) coords)))

(defn get-pos [^BitSet world ^long pos]
  ;{:pre [(<= 0 pos)
  ;       (< pos (count world))]}
  (.get world pos))

(defn set-pos [^BitSet world ^long pos value]
  ;{:pre [(<= 0 pos)
  ;       (< pos (count world))]}
  (.set world pos value))

; print the world: 0 = dead, X = alive
(defn print-world [^BitSet world]
  (let [size (.size world)
        rows-size (int (Math/sqrt size))
        wrld (mapv #(if % "\u2588" " ") (mapv #(get-pos world %) (range size)))]
    (doseq [i (range rows-size)]
      (println (apply str (take rows-size (drop (* i rows-size) wrld)))))))

(defn rand-bool []
  (= 0 (rand-int 2)))

(defn switch-on-random-cell [^BitSet world]
  (let [size (.size world)
        rand-pos (rand-int size)]
    (set-pos world rand-pos true)))

(defn clear-world [^BitSet world]
  (let [size (.size world)]
    (dotimes [i size]
      (set-pos world i false))))

; initialize new world with random bit values
(defn new-world [^long world-size]
  (let [world (BitSet.)]
    (dotimes [i world-size]
      (.set world i (rand-bool)))
    world))

(defn count-trues [^booleans world]
  (count (filter identity world)))

(defn evaluate-cell-life [^long world-count ^BitSet world ^long pos]
  ;{:pre [(<= 0 pos)
  ;       (< pos world-count)]
  ; :post [(is (boolean? %) true)]}
  (let [neighbour-pos (neighbour-pos world-count pos)
        neigh-vals (mapv (partial get-pos world) neighbour-pos)
        total-val (count-trues neigh-vals)
        alive? (get-pos world pos)]
    (cond
      (= 3 total-val) true
      (and alive? (< 1 total-val 4)) true
      :else false)))

(defn update-world! [^BitSet world]
  (let [size (.size world)
        old-world (.clone world)]
    (dotimes [i size]
      (set-pos world i (evaluate-cell-life size old-world i)))))

(defn life-setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/no-stroke)
  {:world (new-world (* grid-size grid-size))
   :total-count (* grid-size grid-size)
   :side-count grid-size
   :pos-range (range (* grid-size grid-size))})

(defn update-life [{^BitSet world :world :as state}]
  (let [old-world (.clone world)]
    (update-world! world)
    (assoc state
           :world world
           :prev-world old-world)))

(defn draw-partition
  [^long total-count ^longs pos-range ^BitSet world]
  ; clear background
  (q/background 0)
  (doseq [pos pos-range]
    (let [[x y] (pos->xy total-count pos)
          cell-value (get-pos world pos)]
      (q/fill (if cell-value 0 255))
      (q/rect (* x cell-size) (* cell-size y) cell-size cell-size))))

(defn draw-life [{^booleans world :world
                  ^long total-count :total-count
                  ^longs pos-range :pos-range}]
  (draw-partition total-count pos-range world))

(defn -main [& _]
  (println "starting animation")
  (q/defsketch life
    :title "game of life"
    :size [500 500]
    :setup life-setup
    :update update-life
    :draw draw-life
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
