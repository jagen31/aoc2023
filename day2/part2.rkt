#lang racket

(require art "lib.rkt")

(perform (draw-seq-performer [800 400] [number-drawer])
  (input-data)
  (interpret day2)
  (translate-cube-game red green blue)
  (run-apl (reduce apl:+ ((each (monad-dfn (reduce apl:* (reduce apl:max ω #:axis 0)))) ((each mix) *ctxt*)))))
