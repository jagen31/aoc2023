#lang racket

(require art art/sequence/lib "lib.rkt")

(perform (draw-seq-performer [800 200] [number-drawer])
  (input-data) (interpret day2)
  (translate-cube-game red green blue)
  (solve-part1) (interpret day2))
