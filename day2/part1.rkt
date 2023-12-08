#lang racket

(require art "lib.rkt")

(perform (draw-seq-performer [800 200] [number-drawer])
  (ix@ 0 (seq (input-data) (interpret day2)))
  (rewrite-in-seq
   (translate-cube-game red green blue)
   (solve-part1) (interpret day2)))
