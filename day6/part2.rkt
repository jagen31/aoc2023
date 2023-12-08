#lang racket

(require art)

(define (calculate-race-range k min)
  (add1 (- k (* 2 (- k (exact-floor (+ (/ k 2) (sqrt (+ (- min) (expt (/ k 2) 2))))))))))

; x(k - x) = min
; kx - x^2 = min
; x^2 - kx = -min
; (x - k/2) ^ 2 = -min + (k/2) ^ 2
; x = k/2 +/- sqrt(-min + (k/2) ^ 2)
;; FIXME jagen maybe manipulate/draw equations in art?

(calculate-race-range 71530 940200)

(calculate-race-range 46857582 2.081412126e14)