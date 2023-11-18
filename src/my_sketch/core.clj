(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.test :refer [is]])
  (:gen-class))

(def grid-size 120)
(def cell-size 10)

(defn pos->xy [^long world-size ^long pos]
  [(mod pos (Math/sqrt world-size)) ; x = position % world size
   (quot pos (Math/sqrt world-size)) ; y = round down to lowest multiple of world size 
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

(defn get-pos [^booleans world ^long pos]
  ;{:pre [(<= 0 pos)
  ;       (< pos (count world))]}
  (nth world pos))

(defn count-trues [^booleans world]
  (count (filter identity world)))

(defn evaluate-cell-life [^long world-count ^booleans world ^long pos]
  ;{:pre [(<= 0 pos)
  ;       (< pos world-count)]
  ; :post [(is (boolean? %) true)]}
  (let [neighbour-pos (neighbour-pos world-count pos)
        neigh-vals (mapv (partial get-pos world) neighbour-pos)
        total-val (count-trues neigh-vals)
        alive? (nth world pos)]
    (cond
      (= 3 total-val) true
      (and alive? (< 1 total-val 4)) true
      :else false)))

(defn rand-bool []
  (= 0 (rand-int 2)))

(defn life-setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/no-stroke)
  {:world (^booleans vec (take (* grid-size grid-size) (repeatedly #(rand-bool))))
   :total-count (* grid-size grid-size)
   :side-count grid-size
   :pos-range (range (* grid-size grid-size))})

(defn update-life [{^booleans world :world
                    ^longs pos-range :pos-range
                    ^long total-count :total-count
                    :as state}]
  (assoc state :world (->> (partition (int (Math/ceil (/ total-count 16))) pos-range)
                           (pmap (fn [partitioned-range]
                                   (mapv #(evaluate-cell-life total-count world %) partitioned-range)))
                           ((comp vec flatten)))))

(defn draw-life [{^booleans world :world
                  ^long total-count :total-count
                  ^longs pos-range :pos-range}]
  ; clear canvas
  (q/background 240)
  (q/fill 255 255 255)
  (doseq [pos pos-range]
    (let [[x y] (pos->xy total-count pos)
          cell-value (get-pos world pos)]
      (q/fill (if cell-value 0 255))
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
