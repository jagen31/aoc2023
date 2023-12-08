#lang racket
(require art "lib.rkt")

(perform (sum-performer)
  (input-data)
  (interpret day1)
  (first-and-last))