#lang racket

(require art art/sequence/lib art/sequence/ravel "lib.rkt")

(perform #;(draw-seq-performer [800 400] [number-drawer]) (quote-performer)
  (input-data)
  (interpret day2)
  (translate-cube-game red green blue)
  (run-apl (reduce apl:+ ((each (monad-dfn (reduce apl:* (reduce apl:max ω #:axis 0)))) ((each mix) *ctxt*)))))
