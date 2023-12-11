#lang racket

(require art art/sequence/lib art/sequence/ravel "lib.rkt")

(perform (draw-seq-performer [1000 200] [number-drawer]) #;(quote-performer)
  (input-data) (solve-part1) (interpret day9))