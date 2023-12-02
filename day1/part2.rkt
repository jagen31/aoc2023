#lang racket
(require art "lib.rkt" "part1.rkt")

(provide (all-defined-out))

(perform sum-performer
  (input-data)
  (interpret day1)
  (normalize-text-nums)
  (debug-perform quote-performer)
  (first-and-last))