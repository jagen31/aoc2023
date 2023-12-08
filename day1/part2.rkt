#lang racket
(require art "lib.rkt")

(perform (sum-performer)
  (input-data)
  (interpret day1)
  (normalize-text-nums)
  (debug-perform (quote-performer))
  (first-and-last))