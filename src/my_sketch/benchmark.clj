(ns my-sketch.benchmark
  (:require [my-sketch.core :refer :all]))

; took ~9000 msec
;(time (dotimes [i 10000]
;        (update-life {:world (into [] (take (* 10 10) (repeatedly #(rand-bool))))})))

; took 6800-7400 msec 
; booleans instead of 0 and 1s and precomputed counts: 6800-7400 msec
; type hints on function arguments: ~6400 ms
; removed pre/post conditions: ~4800ms
; use (doall (pmap ...)) instead of mapv: ~6600ms which is interesting 
; pmap over 16 partitions: ~ 2800ms
; framerate is still pretty bad, but 10000 iterations in 2.8 seconds means 0.28ms per iteration
; 10000 iterations of 10x10 grid is 100 cells, so 0.0028ms per cell

(time (dotimes [_ 10000]
        (update-life {:world (into [] (take (* 10 10) (repeatedly #(rand-bool))))
                      :total-count (* 10 10)
                      :pos-range (range (* 10 10))})))

; 120x120 grid is 14400 cells, 1000 update cycles
; with pmap on 16 partitions: 34_000ms (is 34ms per cycle should get 27 frames per second)
(time (dotimes [_ 1000]
        (update-life {:world (into [] (take (* 120 120) (repeatedly #(rand-bool))))
                      :total-count (* grid-size grid-size)
                      :pos-range (range (* grid-size grid-size))})))
